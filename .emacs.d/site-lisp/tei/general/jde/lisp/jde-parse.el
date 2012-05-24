;;; jde-parse.el
;; $Revision: 1.47 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2000, 2001, 2002 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'semantic-sb)
(require 'semantic-bnf)
(require 'semantic-ctxt)
(require 'avltree)
(require 'thingatpt)
(require 'eieio)
(require 'jde-imenu)                    ; All the imenu stuff is here now!
(require 'efc)

(defcustom jde-auto-parse-enable t
  "Enables automatic reparsing of a Java source buffer.
After you makes changes to the buffer, but only if the buffer is less
than `jde-auto-parse-max-buffer-size'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-auto-parse-buffer-interval 180
  "Time in seconds between buffer change and reparse.
That is between the time between you change a Java source buffer and
the time the JDE reparses the buffer."
  :group 'jde-project
  :type 'number)

(defcustom jde-auto-parse-max-buffer-size 50000
  "Maximum size in bytes of buffers automatically reparsed.
Used when `jde-auto-parse-enable' is non-nil.  Setting the threshold
to 0 causes the JDE to parse a buffer automatically regardless of its
size."
  :group 'jde-project
  :type 'number)

(defvar jde-parse-buffer-needs-reparse-p nil
  "non-nil if buffer changed since last parse.")
(make-variable-buffer-local 'jde-parse-buffer-needs-reparse-p)

(defvar jde-auto-parse-buffer-timer nil
  "Timer used to schedule automatic reparse.")
(make-variable-buffer-local 'jde-auto-parse-buffer-timer)

(defvar jde-parse-casting nil
  "Variable use to determined when the variable was casted")

(defvar jde-parse-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar jde-parse-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

(defvar jde-parse-primitive-types '("byte" "char" "double" "float" 
                                       "int" "long" "short" "boolean")
  "Primitive Java types.")

(defvar jde-parse-attempted-to-import nil
  "Variable use to avoid looping in jde-parse-eval-type-of when
the type of a class could not be found an it tried to import it")

;;(defun t1 () (interactive) (semantic-bovinate-toplevel))
;;(defun t2 () (interactive) (semantic-bovinate-toplevel t))

;;(defun t1 () (interactive) (semantic-find-nonterminal-by-overlay))

(defun jde-parse-after-buffer-changed ()
  "Reparse the current buffer after any change.
Called after `jde-auto-parse-buffer-interval' seconds following a
buffer change if `jde-auto-parse-enable' is non-nil and buffer size
match `jde-auto-parse-max-buffer-size' threshold."
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (if (eq major-mode 'jde-mode)
      (semantic-bovinate-toplevel t)))

(defsubst jde-parse-should-auto-parse-buffer-p ()
  "Return non-nil if the JDE should automatically reparse the buffer."
  (and jde-auto-parse-enable
       (or 
	(<= jde-auto-parse-max-buffer-size 0)
	(< (buffer-size) jde-auto-parse-max-buffer-size))))

;;; Compatibility
(eval-when (compile)
  (if (featurep 'xemacs)
      
      ;; XEmacs
      (defsubst jde-auto-parse-delay ()
        "Return the time in seconds before auto-parse triggering."
        ;; Wake up the timer driver so it updates timer value
        (itimer-driver-wakeup)
        (itimer-value jde-auto-parse-buffer-timer))
    
    ;; GNU Emacs
    (defsubst jde-auto-parse-delay ()
      "Return the time in seconds before auto-parse triggering."
      (- (timer-until jde-auto-parse-buffer-timer (current-time))))
    
    ))

(defun jde-parse-buffer-changed-hook (begin end length)
  "Hook run when Semantic detects a change in the current buffer.
BEGIN and END are respectively the beginning and end of the range of
changed text. LENGTH is the length in bytes of the pre-change text
replaced by that range.  See also `semantic-change-hooks'."
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following 
  ;; guard.
  (when (eq major-mode 'jde-mode)
    (setq jde-parse-buffer-needs-reparse-p t)
    (when (jde-parse-should-auto-parse-buffer-p)
      (if (timerp jde-auto-parse-buffer-timer)
          (let ((rem (jde-auto-parse-delay)))
            (cond
             ((< rem 0)
              ;; Timer has expired, re-schedule a new auto-parse.
              (cancel-timer jde-auto-parse-buffer-timer)
              (setq jde-auto-parse-buffer-timer nil))
             ((< rem 2) ;; less that 2 secs. before auto-parse
              ;; The auto-parse task is about to be triggered when
              ;; this change occurs, so it is delayed to let finish
              ;; typing before re-parsing.
              (timer-inc-time jde-auto-parse-buffer-timer
                              10) ;; wait 10 secs. more.
              (message "Auto parse delayed...")))))
      ;; Schedule a new auto-parse task.
      (or (timerp jde-auto-parse-buffer-timer)
          (setq jde-auto-parse-buffer-timer
                (run-with-timer
                 jde-auto-parse-buffer-interval
                 nil
                 #'jde-parse-after-buffer-changed))))))

(defun jde-parse-buffer-contains-multiple-classes-p ()
  "Return non-nil if buffer contains multiple class definitions."
  (let* ((top-level-classes
	  (semantic-find-nonterminal-by-token 
	   'type 
	   (semantic-bovinate-toplevel)))
	 (top-level-class-count (length top-level-classes)))
    (or 
     (>  top-level-class-count 1)
     (and
      (= top-level-class-count 1)
      (let* ((inner-class-parts (semantic-token-type-parts (car top-level-classes)))
	     (inner-classes 
	      (semantic-find-nonterminal-by-token 
	       'type inner-class-parts)))
	(>= (length inner-classes) 1))))))

;; (defun test ()
;;   (interactive)
;;   (message 
;;    (if (jde-parse-buffer-contains-multiple-classes-p)
;;        "Yes"
;;      "No")))
   
(defvar jde-parse-buffer-contains-multiple-classes-p nil
  "non-nil if buffer contains more than one class definition.")
(make-variable-buffer-local 'jde-parse-buffer-contains-multiple-classes-p)


(defun jde-parse-update-after-parse (tokens)
  "Hook run after Semantic changed the token cache.
TOKENS is the list of tokens, new value of the cache.  It can be nil
when the cache is cleared.
See also `semantic-after-toplevel-cache-change-hook'."
  (if (jde-parse-should-auto-parse-buffer-p)
      (setq jde-parse-buffer-needs-reparse-p nil
            jde-auto-parse-buffer-timer
            (and (timerp jde-auto-parse-buffer-timer)
                 (cancel-timer jde-auto-parse-buffer-timer)
                 nil)))
  (if (car tokens)
      (setq jde-parse-buffer-contains-multiple-classes-p
            (jde-parse-buffer-contains-multiple-classes-p)
            jde-parse-the-method-map
            (jde-parse-method-map "Method map"))))

(defun jde-parse-update-after-partial-parse (tokens)
  "Hook run after Semantic updated the token cache.
TOKENS is the list of updated tokens.
See also `semantic-after-partial-cache-change-hook'."
  (jde-parse-update-after-parse (semantic-bovinate-toplevel)))

(defun jde-get-java-source-buffers ()
  "Return a list of Java source buffers open in the current session."
  (delq nil (mapcar #'(lambda (buffer)
                        (with-current-buffer buffer
                          (if (eq major-mode 'jde-mode)
                              buffer)))
                    (buffer-list))))

(defun jde-get-visible-source-buffers ()
  "Return a list of visible Java source buffers."
  (delq nil (mapcar #'(lambda (buffer) 
                        (if (get-buffer-window buffer 'visible)
                            buffer))
                    (jde-get-java-source-buffers))))

(defun jde-get-selected-source-buffer ()
  (with-current-buffer (window-buffer (selected-window))
    (if (eq major-mode 'jde-mode)
        (current-buffer))))

(defun jde-parse-get-package-name ()
  "Gets the name of the package in which the Java source file in the
current buffer resides."
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
	 (match-beginning 1)
	 (match-end 1))))))

(defun jde-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring 
   class-name 0
   (let ((pos  (position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun jde-parse-get-unqualified-name (name)
  "Gets the last name in a qualified name."   
  (substring name (string-match "[^.]+$" name)))


(defun jde-parse-get-super-class-at-point ()
  (condition-case err
      (let ((superClass "Object")
            (class-re "extends[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
        (save-excursion
          (let ((open-brace-pos
                 (scan-lists (point) -1 1)))
            (when open-brace-pos
              (goto-char open-brace-pos)
              (when (re-search-backward class-re (point-min) t)
                (looking-at class-re)
                (setq superClass (buffer-substring-no-properties
                                  (match-beginning 1)
                                  (match-end 1)))))))
        superClass)
    (error)))

(defun jde-parse-get-innermost-class-at-point ()
  "Get the innermost class containing point.
If point is in a class, this function returns 
(CLASS_NAME . CLASS_POSITION), where CLASS_NAME is the 
name of the class and CLASS_POSITION is the position
of the first character of the class keyword. Otherwise,
this function returns nil."
  ;; (interactive)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (if (looking-at "{")
			(let* ((search-end-pos
			       (if (< left-paren-index (1- left-paren-count))
				   (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				     (if (consp pos)
					 (cdr pos)
				       pos))
				 (point-min)))
                              (case-fold-search nil)
                              (class-re "^[ \t]*\\(\\(public\\|abstract\\|final\\|static\\|strictfp\\|protected\\)[ \t]+\\)*[ \t]*class[ \t]+\\([^ \t\n{]*\\).*")
                              (class-pos (re-search-backward class-re search-end-pos t)))      
                           (if class-pos
			      (progn
				(looking-at class-re)
				(throw
				 'class-found
				 (cons
				  (buffer-substring-no-properties
				   (match-beginning 3)
				   (match-end 3))
				  class-pos))))))))
		  (setq left-paren-index (1+ left-paren-index)))))))))


(defun jde-parse-get-class-at-point () 
  (let ((class-info (jde-parse-get-innermost-class-at-point))
	class-name)
    (while class-info
      (let ((name (car class-info))
	    (pos (cdr class-info)))
	(if (not class-name)
	    (setq class-name name)
	  (setq class-name (concat name "." class-name)))
	(save-excursion
	  (goto-char pos)
	  (setq class-info (jde-parse-get-innermost-class-at-point)))))
    class-name)) 

(defun jde-parse-get-classes-at-point ()
  (interactive)
  (let ((class (jde-parse-get-innermost-class-at-point)))
    (if class (message "%s %s" (car class) (cdr class) ) (message "no class")))
  ;; (goto-char (aref (c-search-uplist-for-classkey (c-parse-state)) 0))
  )


(defun jde-parse-select-qualified-class-name (class &optional prompt)
  "PROMPT the user to select the fully qualified name for CLASS. 
Return the selection."
  (condition-case err
      (let ((names 
	     (jde-jeval-r
	      (format "jde.util.JdeUtilities.getQualifiedName(\"%s\");" class))))
	(if names
	    (if (> (length names) 1)
		(let ((dialog
		       (efc-option-dialog
			"Class Name Dialog"
			:options names
			:text (or prompt "Select class."))))
		  (efc-dialog-show dialog)
		  (oref dialog selection))
	      (car names))
	  (error "Cannot find class %s on the current classpath." class)))
    (error
     (message "%s" (error-message-string err)))))


(defun jde-parse-qualified-name-at-point ()
  "Returns (cons QUALIFIER NAME) where NAME is the symbol at point and
QUALIFIER is the symbol's qualifier. For example, suppose the name at
point is

     int i = error.msg.length()
                   ^
In this case, this function returns (cons \"error.msg\" \"length\").
This function works only for qualified names that do not contain
white space. It returns null if there is no qualified name at point."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
       (thing-at-point-looking-at "[^ \n\t();,:+]+")
      (let ((qualified-name 
	     (buffer-substring-no-properties
	      (match-beginning 0)
	      (match-end 0))))
	(string-match "\\(.+[.]\\)*\\([^.]+\\)" qualified-name)
	(let ((qualifier (if (match-beginning 1)
			     (substring qualified-name 
					(match-beginning 1) (match-end 1))))
	      (name (substring qualified-name 
			       (match-beginning 2) (match-end 2))))
	  (if qualifier
	      (setq qualifier (substring qualifier 0 (1- (length qualifier)))))
	  (cons qualifier name))))))


(defun jde-parse-get-buffer-class ()
  "Get the fully qualified name of the class of this buffer."
  (if (eq major-mode 'jde-mode)
      (let ((package-name (jde-parse-get-package-name))
	    (class-name (file-name-sans-extension 
			 (file-name-nondirectory (buffer-file-name)))))
	(if package-name
	    (concat package-name "." class-name)
	  class-name))
    (error "Not a Java source buffer.")))


(defun jde-parse-double-backslashes (name)
  (mapconcat (lambda (x) (if (eq x ?\\)
			     "\\\\"
			   (string x)))
	     name ""))

(defun jde-parse-valid-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (let ((case-fold-search nil))
      (if (looking-at (concat "\\([]?[A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+" 
                              (jde-parse-double-backslashes varname) 
                              "[]?[ \t\n\r]*[),;=]"))
          (let ((type (match-string 1))
                (type-pos (match-beginning 1)))
            (goto-char type-pos)
            ;;  Check for following case.
            ;;     Object table
            ;;    //representing objects after all updates.
            ;;    table = new Truc();
            ;;    table.
            ;;  Avoid false hit on updates.
            (if (not (or 
                      (jde-parse-comment-or-quoted-p)
                      (string= type "instanceof")
                      (string= type "return")))
                type))
        nil))))

(defun jde-parse-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a cons of two strings.  The first is a string containing
the name of the type, or nil if it cannot be found. The second is a string
containing any qualifying text that precedes the class name, which is nil
if no text that looks like an identifier precedes the type name.  This
function does not give the fully-qualified java class name, it just returns
the type as it is declared, and a qualifier that might be the package or
the containing/outer class of the declared type."
  (save-excursion
    (let (found res pos orgpt resname foundpt qualifier)
      (setq orgpt (point))
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(setq foundpt (point))
	(goto-char pos)
	(forward-char -1)
	(if (and (not (null resname))
                 (not (jde-parse-keywordp resname)))
	    (progn (setq res resname)
		   (setq found t))))
      
      (goto-char orgpt)

      (while (and (not found)
		  (search-forward name nil t))
	(setq pos (point))
	(backward-word 2)
	(setq resname (jde-parse-valid-declaration-at (point) name))
	(setq foundpt (point))
	(goto-char pos)
	(forward-char 1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))

      ;; now check for qualifying identifier. Note: reuses 
      ;; jde-parse-qualified-name-at-point for simplicity, not efficiency
      (if found
	  (let (qualname)
	    (goto-char foundpt)
	    (setq qualname (jde-parse-qualified-name-at-point))
	    (setq qualifier (car qualname))
	    (setq res (cdr qualname))))

      (cons res qualifier))))


(defun jde-display-parse-error (error)
  (let* ((parser-buffer-name "*Java Parser*")
	 (buf (get-buffer parser-buffer-name))) 
    (if (not buf)
	(setq buf (get-buffer-create parser-buffer-name)))
    (set-buffer buf)
    (erase-buffer)
    (insert error)
    (pop-to-buffer buf)))

(defun jde-parse ()
  "*Parses the Java source file displayed in the current buffer.
If the source file parses successfully, this command displays
a success message in the minibuffer. Otherwise, it displays an error
message in the Java Parser buffer. If the Java Parser buffer does
not exist, this command creates it.

Note. This command uses an external Java parser implemented in
Java to parse Java source files. This command uses the JDE's integrated
Java source interpreter, the BeanShell, to invoke the parser. If the
BeanShell is not running, this command starts the BeanShell. Thus,
the first time you invoke the parser you may notice a slight delay
before getting a response. Thereafter, the response should be very
fast."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((parse-error
	 (jde-jeval-r (concat "jde.parser.ParserMain.parseFile(\"" (buffer-file-name) "\");"))))
    (if parse-error
	(jde-display-parse-error parse-error)
      (message "Parsed %s successfully" (buffer-name)))))


;; Thanks to Eric D. Friedman <friedman@lmi.net> for this function.
(defun jde-parse-comment-or-quoted-p ()
  "Returns t if point is in a comment or a quoted string. nil otherwise"
  (interactive "p")
  ;; limit our analysis to the current line.
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (if
        (or
         ;; are we in a javadoc comment?
         (save-excursion
           (re-search-backward
            "^[ \t]*/?\\*"
            beg t))
         ;; are we in a '//' or a '/*' style comment?
         ;; note that /* or /** on a line with only leading whitespace
         ;; will have matched in the previous regex.  We check again here
         ;; because the above case needs to allow for a line with
         ;; the continuation of a comment (using only a '*') whereas this
         ;; requires the presence of a '/' in front of the '*' so as to
         ;; distinguish a comment from a '*' operator.
         ;; To make a long story short,
         ;; the first regex matches
         ;;   /* a comment */
         ;; and
         ;; /**
         ;;  * a comment
         ;;  */
         ;; while the second one matches
         ;; System.out.println(foo); /* a comment */
         ;; but not
         ;; i = 3 * 5;
         ;; if you do something like following, you're on your own:
         ;; i = 3
         ;;       * 5; 
         ;; Suggestions for improving the robustness of this algorithm
         ;; gratefully accepted.
         (save-excursion
           (re-search-backward
            "\\(//\\|/\\*\\)"
            beg t))
         ;; are we in a quoted string?
         (save-excursion
           (re-search-backward
            "\"";;
            beg t)))
        t;; return true if we had any matches; nil otherwise
      nil)))

(defun jde-parse-get-method-at-point (&optional position)
  "Gets the method at POSITION, if specified, otherwise at point.
Returns (CLASS_NAME . METHOD_NAME) if the specified position is
in a method; otherwise, nil."
  ;; Define an internal function that recursively searches a class
  ;; and its subclasses for a method containing point.
  (flet ((search-class 
	  (class pos)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	    ;; Is point in a method of a subclass of this class?
	    (loop for subclass in class-subclasses do
		  (search-class subclass pos))

	    ;; Is point in any of the methods of this class?
	    (loop for method in class-methods do
		  (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		    (when (and (>= pos method-start) 
			       (<= pos method-end))
		      (throw 'found (cons (cons class-name method-name)
					  (cons method-start method-end)))))))))  
		       
    (let* ((pos (if position position (point)))
	   (tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (catch 'found
	(loop for class in classes
	      do (search-class class pos))))))



(defclass jde-avl-tree ()
  ((tree        :initarg tree
	        :type list
	        :documentation
	        "The tree")
   (compare-fcn :initarg compare-fcn
		:type function
		;; :initform <
		:documentation    
		"Compare function."))
  "Balanced binary tree.")

(defmethod initialize-instance ((this jde-avl-tree) &rest fields)
  "Constructor for binary balanced tree."
  
  ;; Call parent initializer
  (call-next-method)

  (assert (typep  (oref this compare-fcn)  'function))

  (oset this  tree (avltree-create (oref this compare-fcn))))

(defmethod jde-avl-tree-add ((this jde-avl-tree) item)
  "Inserts ITEM in this tree."
  (avltree-enter (oref this tree) item))

(defmethod jde-avl-tree-delete ((this jde-avl-tree) item)
  "Deletes ITEM from THIS tree."
  (avltree-delete (oref this tree) item))

(defmethod jde-avl-tree-is-empty ((this jde-avl-tree))
  "Return t if THIS tree is empty, otherwise return nil."
  (avltree-empty (oref this tree)))

(defmethod jde-avl-tree-find ((this jde-avl-tree) item)
  "Return the element in THIS tree that matches item."
  (avltree-member (oref this tree) item))

(defmethod jde-avl-tree-map ((this jde-avl-tree) map-function)
  "Applies MAP-FUNCTION to all elements of THIS tree."
  (avltree-map map-function (oref this tree)))

(defmethod jde-avl-tree-first ((this jde-avl-tree))
  "Return the first item in THIS tree."
  (avltree-first (oref this tree)))
  
(defmethod jde-avl-tree-last ((this jde-avl-tree))
  "Return the last item in THIS tree."
  (avltree-last (oref this tree)))

(defmethod jde-avl-tree-copy ((this jde-avl-tree))
  "Return a copy of THIS tree."
  (avltree-copy (oref this tree)))

(defmethod jde-avl-tree-flatten ((this jde-avl-tree))
  "Return a sorted list containing all elements of THIS tree."
  (avltree-flatten (oref this tree)))

(defmethod jde-avl-tree-size ((this jde-avl-tree))
  "Return the number of elements in THIS tree."
  (avltree-size (oref this tree)))

(defmethod jde-avl-tree-clear ((this jde-avl-tree))
  "Delete all elements of THIS tree."
  (avltree-clear (oref this tree)))

(defclass jde-parse-method-map (jde-avl-tree) 
  ()
  "Map of the methods in the current buffer.")


(defun jde-parse-method-map-compare-fcn (m1 m2)
  (and 
   (< (car (cdr m1)) (car (cdr m2)))
   (< (cdr (cdr m1)) (car (cdr m2)))))

(defmethod initialize-instance ((this jde-parse-method-map) &rest fields)
  "Constructor for method map."

  (oset  this compare-fcn 'jde-parse-method-map-compare-fcn)

					;   (oset 
					;    this 
					;    compare-fcn
					;     (lambda (m1 m2)
					;       (and 
					;        (< (car (cdr m1)) (car (cdr m2)))
					;        (< (cdr (cdr m1)) (car (cdr m2))))))

  ;; Call parent initializer.
  (call-next-method)

  (flet ((add-methods 
	  (class)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	    ;; Add methods of subclasses
	    (loop for subclass in class-subclasses do
		  (add-methods subclass))

	    ;; Add methods of this class?
	    (loop for method in class-methods do
		  (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		    (jde-avl-tree-add 
		     this
		     (cons
		      (cons class-name method-name)
		      (cons method-start method-end))))))))
		       
    (let* ((tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (loop for class in classes do 
	    (add-methods class)))))

(defmethod jde-parse-method-map-get-method-at ((this jde-parse-method-map) &optional pos)
  "Get the method at POS, if specified, otherwise, at point."
  (let ((p (if pos pos (point))))
    (jde-avl-tree-find this (cons (cons "" "") (cons p p)))))

(defvar jde-parse-the-method-map nil
  "Map of methods defined in this buffer sorted by location.")
(make-variable-buffer-local 'jde-parse-the-method-map)


(defun jde-current-buffer-exact-name-match-p (tag)
  (and (tag-exact-match-p tag)
       (equal (buffer-file-name (window-buffer (selected-window))) 
	      (file-of-tag))))

(defun jde-etags-recognize-tags-table () ; see etags-recognize-tags-table
  (let ((recognized (etags-recognize-tags-table)))
    (if recognized 
	;; prefer exact match in current buffer to other files
	(setq find-tag-tag-order '(jde-current-buffer-exact-name-match-p
				   tag-exact-file-name-match-p
				   tag-exact-match-p
				   ))
      recognized)))

(defun jde-parse-java-variable-at-point () 
  "Returns a list (VAR PARTIAL) where VAR.PARTIAL is the partially completed
method or field name at point. For example, suppose obj.f1.ge were the name
at point. This function would return the list (obj.f1 ge)."
  (save-excursion
    (let (start middle-point varname curcar dot (dot-offset 0) found
                (original-point (point)) 
                intermediate-point beg-point first-part second-part
                first-paren cast-type second-paren
                args (offset 0) (bracket-count 0) (paren-count 0))

      ;; Move cursor to the beginning of the partially
      ;; completed part of the expression, e.g., move point
      ;; from
      ;;
      ;;   obj.f1.ge   to obj.f1.ge
      ;;            ^           ^
      ;;   obj.f1.get(int,int)  to obj.f1.get(int,int)
      ;;                      ^              ^
      (setq curcar (char-before))
      (if curcar
          (progn
            (while (null found)
              (cond 
               ((or (and (>= curcar ?a) (<= curcar ?z)) ; a-z
                    (and (>= curcar ?A) (<= curcar ?Z)) ; A-z
                    (and (>= curcar ?0) (<= curcar ?9))
                    (>= curcar 127)
                    (member curcar '(?$ ?_ ?\\))) ;; _ \
                (forward-char -1))
               ((eq ?. curcar)
                (setq dot-offset 1)
                (if (eq ?\) (char-before (- (point) 1)))
                    (progn
                      (forward-char -2)
                      (setq first-paren (point))
                      (setq second-paren (jde-parse-match-paren-position))
                      (setq offset (+ (- first-paren second-paren) 1))
                      (forward-char 2)
                      (setq found (point)))
                  (setq found (point))))
               (t
                (setq found (point))))
              (setq curcar (char-before)))
            (setq intermediate-point found)
           ;; Now move point to the beginning of the expression, e.g.,
            ;; from
            ;;
            ;;  obj.f1.ge
            ;;        ^
            ;; to
            ;;
            ;;  obj.f1.ge
            ;; ^
            ;;
            (progn 
              (setq curcar (char-before))
              (while (or (and (>= curcar ?a) (<= curcar ?z))
                         (and (>= curcar ?A) (<= curcar ?Z))
                         (and (>= curcar ?0) (<= curcar ?9))
                         (>= curcar 127)
                         (and (eq curcar ? ) (or (< 0 paren-count)
                                                 (< 0 bracket-count)))
                         (and (member curcar '(?$ ?\" ?\. ?\_ ?\\ ?\( ?\) ?\, ?\[ ?\]))
                              (if (eq curcar ?\[)
                                  (> bracket-count 0)
                                t)))
                (cond 
                 ((eq curcar ?\))
                  (progn
                    (forward-char -1)
                    (goto-char (jde-parse-match-paren-position))))
                 ((eq curcar ?\( )
                  (setq paren-count (1- paren-count)))
                 ((eq curcar ?\] )
                  (setq bracket-count (1+ bracket-count)))
                 ((eq curcar ?\[ )
                  (setq bracket-count (1- bracket-count))))
                (forward-char -1)
                (setq curcar (char-before)))
        
              (setq beg-point (point))
              (set-marker jde-parse-current-beginning intermediate-point)
              (set-marker jde-parse-current-end original-point)
              (setq middle-point (- intermediate-point dot-offset offset))
              (setq first-part
                    (buffer-substring-no-properties beg-point middle-point))
              (setq first-part (jde-parse-isolate-to-parse first-part))
        
     ;;replacing newline by empty strings new lines seems to break the
              ;;beanshell
              (while (string-match "\n" first-part)
                (setq first-part (replace-match "" nil nil first-part))) 
        
        ;;replacing extra spaces for "". This done to reduce the space
              ;;that the completion title takes
              (while (string-match " " first-part)
                (setq first-part (replace-match "" nil nil first-part))) 
        
              (setq second-part
                    (buffer-substring-no-properties
                     intermediate-point original-point))

              ;;Checking for casting
              ;; ((Object) obj).ge
              (if (and (not cast-type)
                       (string= first-part "")
                       (eq (char-before (+ 1 middle-point)) ?\()
                       (eq (char-before (+ 2 middle-point)) ?\())
                  (save-excursion 
                    (goto-char (+ middle-point 1))
                    (setq first-paren (point))
                    (setq second-paren (jde-parse-match-paren-position))
                    (setq cast-type (buffer-substring-no-properties
                                     (+ 1 first-paren) second-paren))))
        
              (if cast-type
                  (progn
                    (setq jde-parse-casting t)
                    (list cast-type second-part))
                (progn
                  (setq jde-parse-casting nil)
                  (list first-part second-part))))
            )))))

(defun jde-parse-isolate-to-parse (s)
  "Returns the right expression that needs completion in S." 
  (let* ((index (length s)) stop (paren 0) curcar final-string
         (inside-quotes nil))
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar);;Checking if we are inside double quotes
        (if (not (eq ?\\ (aref s (- index 1))));;if the quote is not escape
            (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
        (if (not inside-quotes)
            (setq paren (1+ paren))))
       ((eq ?\( curcar)
        (if (not inside-quotes)
            (setq paren (1- paren)))))
      (if (or (< paren 0)
	      (and (eq curcar ?\,) (<= paren 0)))
	  (setq stop t)))
    (if stop
	(setq index (1+ index)))
    (setq final-string (substring s index))
    (if (and (not (string= final-string ""))
             (string= "(" (substring final-string 0 1)))
        (let* ((closing-paren (string-match ")" final-string))
               (closing (string-match ")." final-string (+ 1 closing-paren))))
          (setq final-string
                (concat (substring final-string 2 closing-paren)
                        "."
                        (substring final-string (+ closing 2))))))
    final-string))
(defun jde-parse-match-paren-position ()
  "Returns the position of the parenthesis match"
  (let ((current (point))
        match)
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1)))
    (setq match (point))
    (goto-char current)
    match))

(defun jde-parse-eval-type-of (expr)
  "Get type of EXPR. This function returns a class name or builtin
Java type name, e.g., int."
  (if expr
      (let ((class-at-point (jde-parse-get-class-at-point))
            (super-at-point (jde-parse-get-super-class-at-point))
            qualified-name chop-pos temp answer)
        (setq answer
              (cond
               ;;If it's number returns an int
               ((integerp expr)
                "int")
               ;;If it's 001L or 134l return long
               ((if (and (integerp (substring expr 0 (- (length expr) 1)))
                         (or (string= "L" (substring expr (- (length expr) 1)
                                                     (length expr)))
                             (string= "l" (substring expr (- (length expr) 1)
                                                     (length expr)))))
                    "long"))
               ;;If it's a floating point return float
               ((floatp expr)
                "double")
               ;;If it's 000F or 1234f return float
               ((if (and (floatp (substring expr 0 (- (length expr) 1)))
                         (or (string= "F" (substring expr (- (length expr) 1)
                                                     (length expr)))
                             (string= "f" (substring expr (- (length expr) 1)
                                                     (length expr)))))
                    "float"))
               ((string-match "false\\|true" expr)
                "boolean")
               ;;Checking if it's a character
               ((string= "'" (substring expr 0 1))
                "char")
               ;; If it's "this", we return the class name of the class we code in
               ((and class-at-point (string= "this" expr))
                (jde-parse-get-qualified-name class-at-point))
               ;; If it's "super", we return the super class
               ;;name of the class we code in
               ((and super-at-point (string= "super" expr))
                (jde-parse-get-qualified-name super-at-point))
               ;;if it's a class name, done
               ((setq qualified-name (jde-parse-get-qualified-name expr))
                qualified-name)

               ;;check if it's an inner class
               ((and expr
                     class-at-point
                     (string= "this.this" expr)
                     (setq qualified-name (jde-parse-get-inner-class
                                           class-at-point)))
                qualified-name)
                           
               (t
                (let (to-complete
                      (last-char 
                       (aref expr (- (length expr) 1 ))))
                  ;; If it ends with a parenthesis
                  (cond
                   ((eq last-char ?\))
                    (let* ((result
                            (jde-parse-isolate-before-matching-of-last-car
                             expr))
                           (temp (if (not (string= "this.this" result))
                                     (jde-parse-split-by-dots result)))
                           to-complete)
                      (if temp
                          (jde-parse-find-completion-for-pair temp)
                        ;;we need exact completion here
                        (jde-parse-find-completion-for-pair
                         (list "this" result) nil jde-complete-private))

                      ;;if the previous did not work try only result
                      (if (not jde-complete-current-list)
                          (jde-parse-find-completion-for-pair (list result "")))
                 
                      ;;if the previous did not work try again 
                      (setq qualified-name
                            (jde-parse-get-qualified-name result))
                      (if qualified-name
                          qualified-name
                        (if jde-complete-current-list
                            (progn
                              (setq to-complete
                                    (car (car jde-complete-current-list)))
                              (setq chop-pos (string-match " : " to-complete))
                              (substring to-complete (+ chop-pos 3)))
                          (error "Could not find type of %s" expr)))))

                   ;;if it's an array
                   ((eq last-char ?\])
                    (let ((temp (jde-parse-eval-type-of 
                                 (jde-parse-isolate-before-matching-of-last-car
                                  expr))))
                      (jde-parse-get-component-type-of-array-class temp)))

                   ;;we look for atoms if expr is splittable by dots
                   ((setq temp (if (not (string= "this.this" expr))
                                   (jde-parse-split-by-dots expr)))
                    ;;we need exact completion here
                    (jde-parse-find-completion-for-pair temp t)
                    (if jde-complete-current-list
                        (progn
                          (setq to-complete (car (car
                                                  jde-complete-current-list)))
                          (setq chop-pos (string-match " : " to-complete))
                          (jde-parse-get-qualified-name
                           (substring to-complete (+ chop-pos 3))))
                      
                      nil))
                   (t
                    ;; See if it's declared somewhere in this buffer.
                    (let (parsed-type result result-qualifier)
                      (setq parsed-type (jde-parse-declared-type-of expr))
                      (setq result (car parsed-type))
                      (setq result-qualifier (cdr parsed-type))

                      (if result
                          (let ((count 0) type)
                            (while (string-match ".*\\[\\]" result)
                              (setq result (substring result 0
                                                      (- (length result) 2)))
                              (setq count (1+ count)))

                            (let (work)
                              (setq type
                                    (cond
                                  ;; handle primitive types, e.g., int
                                     ((member result jde-parse-primitive-types)
                                      result)
                             ;; quickly make sure fully qualified name
                                     ;;doesn't exist
                                     ((and result-qualifier 
                                           (jde-parse-class-exists 
                                            (setq work (concat result-qualifier
                                                               "."
                                                               result))))
                                      work)
                                     ;; then check for inner classes
                                     ((setq work
                                            (jde-parse-get-inner-class-name
                                             result result-qualifier))
                                      work)
                               ;; otherwise use unqualified class name
                                     (t
                                      (jde-parse-get-qualified-name result)))))

                            (if type
                                (progn
                                  (while (> count 0)
                                    (setq type (concat type "[]"))
                                    (setq count (1- count)))
                                  (jde-parse-transform-array-classes-names
                                   type))
                              (if (y-or-n-p
                                   (format (concat "Could not find type of %s"
                                                   " Attempt to import %s? ") 
                                           expr result))
                                  (progn
                                    ;; import
                                    (jde-import-find-and-import result)
                                    ;; recursive call of eval-type-of
                                    (jde-parse-eval-type-of expr))
                                (error "Could not find type of %s" result))))
                        (if (and jde-parse-casting 
                                 (null jde-parse-attempted-to-import)
                                 (y-or-n-p 
                                  (format (concat "Could not find type of %s"
                                                  " Attempt to import %s? ")
                                          expr expr)))
                            (progn
                              (setq jde-parse-attempted-to-import t)
                              (setq jde-parse-casting nil)
                              (jde-import-find-and-import expr)
                              (jde-parse-eval-type-of expr))
                          (progn
                            (setq jde-parse-attempted-to-import nil)
                            nil))))))))))
        answer)))

(defun jde-parse-convert-args-to-types (args) 
  "Converts something like this (10, 10) to (int, int)"
  (let* ((answer "(")
         (first-time t)
         (temp (substring args 1 (- (length args) 1)));;Striping the parens
         (lst (split-string temp ", ?"))
         tmp)
    (while lst
      (setq tmp (car lst))
      (if (not first-time)
          (setq answer (concat answer ", "))
        (setq first-time nil))
      (setq answer (concat answer 
                           (jde-parse-eval-type-of tmp)))
      (setq lst (cdr lst)))
    (setq answer (concat answer ")"))
    answer))

(defun jde-parse-transform-array-classes-names (name)
  (let (result)
    (while (string-match ".*\\[\\]" name)
      (setq name (substring name 0 (- (length name) 2 ))) 
      (setq result (concat "[" result)))
    (if result
        (progn
          (cond
           ((string= name "byte")
            (setq result (concat result "B")))
           ((string= name "char")
            (setq result (concat result "C")))
           ((string= name "double")
            (setq result (concat result "D")))
           ((string= name "float")
            (setq result (concat result "F")))
           ((string= name "int")
            (setq result (concat result "I")))
           ((string= name "long")
            (setq result (concat result "J")))
           ((string= name "short")
            (setq result (concat result "S")))
           ((string= name "boolean")
            (setq result (concat result "Z")))
           (t
            (setq result (concat result "L" name ";"))))
          result)
      name)))

(defun jde-parse-get-component-type-of-array-class (name)
  (if (string= "[" (substring name 0 1))
      (let (result)
        (setq result
              (jde-jeval
               (concat "System.out.println( Class.forName(\""
                       name
                       "\").getComponentType().getName()) ;"))) ;;removed \n
        (substring result 0 (- (length result) 1)))
    name))

;; Modified `jde-parse-import-list' to use semantic parser table
(defun jde-split-import-token (token)
  "Helper function used by `jde-parse-import-list' which return a
list (PACKAGE-DOT CLASS-OR-STAR) from given semantic 'include (that
is Java import) TOKEN.
For example:
  : (jde-split-import-token \"java.util.Hashtable\")
  > (\"java.util.\" . \"Hashtable\")
  : (jde-split-import-token \"java.lang.*\")
  > (\"java.lang.\" . \"*\")
  : (jde-split-import-token \"test\")
  > (\"test.\" . \"*\")"
  (let* ((import      (semantic-token-name token))
         (match-point (string-match "\\." import))
	 split-point)
    (while match-point
      (setq split-point (1+ match-point)
            match-point (string-match "\\." import split-point)))
    (if split-point
        (list (substring import 0 split-point)
              (substring import split-point))
      (list (concat import ".")
            "*"))))

(defun jde-parse-import-list ()
  "Return the list of Java packages declared in the current buffer.
It uses the semantic parser table to find the 'package' and 'import'
statements. It implicitly adds the java.lang.* package. See also
`jde-split-import-token'."
  (let* ((tokens   (semantic-bovinate-toplevel t))
         (packages (semantic-find-nonterminal-by-token 'package tokens))
         (imports  (semantic-find-nonterminal-by-token 'include tokens))
         lst)
    (setq lst (append
               (mapcar (function
                        (lambda (token)
                          (list
                           (concat (semantic-token-name token) ".")
                           "*")))
                       packages)
              (mapcar 'jde-split-import-token
                       imports)))
    (or (member "java.lang.*" lst)
        (setq lst (append lst '(("java.lang." "*")))))
    lst))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Get the fully qualified name of the class NAME, using the import
;; list. It returns a string if the fqn was found, or null otherwise.
;; This is more capable than jde-complete-guess-type-of because it
;; uses the beanshell to determine if an import statement with a
;; wildcard contains the unqualified class name passed to this
;; function.
(defun jde-parse-get-qualified-name (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or null otherwise."
  (if (jde-parse-class-exists name)
      name
    (let ((importlist (jde-parse-import-list))
          shortname fullname tmp result)
      (while (and importlist (null result))
	(setq tmp (car importlist))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(cond 
	 ((and (string= "*" shortname) (jde-parse-class-exists fullname))
	  (setq result fullname))
	 ((string= name shortname)
	  (setq result fullname))
	 (t 
	  (setq importlist (cdr importlist)))))
      result)))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Returns t if the fully qualified class name can be found in the
;; classpath, nil otherwise
(defun jde-parse-class-exists (name)
  ;;replacing double quotes by empty strings double quotes seems to break
  ;;the beanshell
  (while (string-match "\"" name)
    (setq name (replace-match "" nil nil name))) 
  
  ;;replacing back slaches by empty strings backlashes causes beanshell problem
  (while (string-match "\\\\" name)
    (setq name (replace-match "" nil nil name))) 
  (jde-jeval-r (concat "jde.util.JdeUtilities.classExists(\"" name "\");")))

(defun jde-parse-get-inner-class (expr) 
  "Takes a single argument like B.A and split it up in a name
and qualifer. The name and the qualifier are then use as arguments 
to the function `jde-parse-get-inner-class-name'"
  (let (name qualifier (pos (string-match "\\." expr)))
    (if pos
        (progn
          (setq name (substring expr (+ 1 pos)))
          (setq qualifier (substring expr 0 pos))))
    (jde-parse-get-inner-class-name name qualifier)))

;; Tries to divine inner class via the following algorithm:
;; if no qualifier (e.g. A), then try
;;   <this-class>$A
;; if qualifier (e.g. D.C.B.A), then try the following in order:
;;   <this-class>$D$C$B$A
;;   <super-types>$D$C$B$A (NOT YET IMPLEMENTED)
;;   D$C$B$A           i.e  D$C$B$A in default, current, or imported package
;;   D + C$B$A         i.e  D in imports or local/default package with
;;                          a C$B$A inner class
;;   D.C.B$A           i.e. B$A in package D.C
;;   D.C$B$A           i.e. C$B$A in package D
(defun jde-parse-get-inner-class-name (name qualifier)
  "Tries to find an inner class with the class name of name.  If qualifier
is nil, the inner class is only searched on current class at point.  If
qualifier is not nil, a wider array of possible inner classes will
searched.  Returns nil if not found; a fully qualified inner class name
otherwise."
  (let (class-name this-full-class-name result)
    (setq this-full-class-name (jde-parse-get-qualified-name
                                (jde-parse-get-class-at-point)))
    (cond
     (qualifier
      (let ((work (concat qualifier "$" name)))
        (setq work (subst-char-in-string ?. ?\$ work))

        ;; check against this$D$C$B$A
        (setq class-name (concat this-full-class-name "$" work))
        (if (not (jde-parse-class-exists class-name))
            (let (dot-count index first-part remaining-part)
              (setq class-name nil)
             
              ;; check against D$C$B$A (default and imported)
              (print "Zero dots" (get-buffer "*scratch*"))
              (setq class-name (jde-parse-get-qualified-name work))

              ;; check remaining semi-qualified variants
              (setq dot-count 1)
              (while (and (null class-name)
                          (setq index (string-match "\\$" work)))

                (setq first-part (substring work 0 index))
                (setq remaining-part (if (< index (length work))
                                         (substring work (+ 1 index))
                                       nil))
                (cond
                 ;; check against C$B$A on class D
                 ((= 1 dot-count)
                  (setq class-name (jde-parse-get-qualified-name
                                    first-part))
                  (if (not (null class-name))
                      (progn
                        (setq class-name (concat class-name
                                                 "$"
                                                 remaining-part))
                        (if (null (jde-parse-class-exists class-name))
                            (setq class-name nil)))))

                 ;; just check if it exists (ignoring imports)
                 (t
                  (setq class-name (concat first-part "." remaining-part))
                  (if (null (jde-parse-class-exists class-name))
                      ;; lastly check if it exists in this package
                      (progn
                        (setq class-name (concat (jde-parse-get-package-name)
                                                 "."
                                                 work))
                        (if (not (jde-parse-class-exists class-name))
                            (setq class-name nil))))))

                (setq work (concat first-part "." remaining-part))
                (setq dot-count (1+ dot-count)))

              class-name))))

     ;=; otherwise, no qualifier...check against this$D
     ((jde-parse-class-exists (setq class-name
                                    (concat this-full-class-name "$" name)))
      class-name))))

;; Can this function be reimplemented to use regular expressions?
(defun jde-parse-isolate-before-matching-of-last-car (s)
  "Returns the right expression that needs completion in S." 
  (let* (final-string (index (length s)) stop (paren 0) (bracket 0) curcar
                      (inside-quotes nil))
    (while (and (> index 0)
		(not stop))     
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar);;Checking if we are inside double quotes
        (if (not (eq ?\\ (aref s (- index 1))));;if the quote is not escape
            (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
        (if (not inside-quotes)
            (setq paren (1+ paren))))
       ((eq ?\( curcar)
        (if (not inside-quotes)
            (setq paren (1- paren))))
       ((eq ?\] curcar)
        (if (not inside-quotes)
            (setq bracket (1+ bracket))))
       ((eq ?\[ curcar)
        (if (not inside-quotes)
            (setq bracket (1- bracket)))))
      (if (and (= paren 0)
	       (= bracket 0)) 
	  (setq stop t)))
    (setq final-string (substring s 0 index))
    (if (and (string= "" final-string)
             (string= "((" (substring s 0 2)))
        (let ((closing-paren (string-match ")" s)))
          (setq final-string (substring s 2 closing-paren))))
    final-string))

(defun jde-parse-split-by-dots (s)
  "Return a list containing the longest substring of S that ends with a dot,
 and the rest.But removes the intermediate(=last) dot."
  ;;we now isolate the last atom after a dot and the beginning
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (list (match-string 1 s) (match-string 2 s))
    nil))

(defun jde-parse-find-completion-for-pair (pair &optional exact-completion
                                                access-level)
  (jde-complete-find-completion-for-pair pair exact-completion access-level))

(defun jde-parse-keywordp (variable) 
  "Checks if VARIABLE is a Java keyword, for performance reasons this only checks if the VARIABLE is an if or an else, since those are the ones that seem to be causing a problem"
  (or (string= variable "if")
      (string= variable "else")))

(provide 'jde-parse)

;; $Log: jde-parse.el,v $
;; Revision 1.47  2002/10/05 12:22:51  jslopez
;; Fixes bug in completion when jde-complete-display-qualified-types is set to
;; nil.
;;
;; Revision 1.46  2002/07/18 04:30:01  jslopez
;; Fixes regression bug in jde-parse-valid-declaration-at.
;;
;; Revision 1.45  2002/07/18 04:05:23  jslopez
;; Fixes bug that was causing arrays defined as
;; String s[]; not to be parse correctly.
;;
;; Revision 1.44  2002/07/12 12:11:10  jslopez
;; Adds jde-parse-keywordp.
;; Fixes bug in method jde-parse-declared-type-of.
;; It was selecting the java keyword "else" as a class for a variable.
;;
;; Revision 1.43  2002/05/12 06:43:21  paulk
;; Added jde-parse-convert-args-to-types and jde-parse-get-buffer-class
;; functions.
;;
;; Revision 1.42  2002/04/16 03:17:06  jslopez
;; Integrates jde-open-source.
;;
;; Revision 1.41  2002/03/05 10:41:32  paulk
;; Minor tweak in jde-parse-get-unqualified-name.
;;
;; Revision 1.40  2002/01/28 07:35:27  paulk
;; Updated jde-parse-select-qualified-class-name to use the efc-option-dialog class.
;;
;; Revision 1.39  2001/11/30 02:46:40  jslopez
;; Modifies jde-parse-get-super-class-at-point to return
;; nil instead of a scan-list error.
;;
;; Revision 1.38  2001/11/28 22:29:53  jslopez
;; Fixes compilation messages.
;;
;; Revision 1.37  2001/11/09 03:14:40  jslopez
;; Fixes bug caused by doing case insensitve searches in the method
;; jde-parse-valid-declaration-at.
;; i.e
;; String level = argConfig.getProperty(OPTION_LOG_LEVEL);
;; Level temp = Level.parse(level.toUpperCase());
;; 				  ^
;; Looking for the type of Level returned String erroneously.
;;
;; Revision 1.36  2001/10/26 06:40:03  paulk
;; Updated jde-parse-select-qualified-class-name to reflect the
;; new modal behavior of jde-option-dialog.
;;
;; Revision 1.35  2001/09/16 17:51:17  paulk
;; - David Ponce rewrote the auto-parse stuff so it can benefit from the new partial
;; re-parse mechanism in Semantic 1.4.  Thus now if you change something
;; inside some tokens (variables, methods, etc.), only these tokens are
;; re-parsed.  Full re-parse occurs only when necessary.  This should
;; speed up auto-parse in many case.
;;
;;
;; -David also changed the way auto-parse is triggered.  When the buffer is modified
;; `jde-parse-buffer-changed-hook' check the auto-parse timer to see if
;; the auto-parse task is about to be triggered (< 2 secs).  If so it
;; delay the triggering of auto-parse (+ 10 secs) to let you finish
;; typing before doing a re-parse. And so on.
;;
;; - David also made some minor doc string fixes (checkdoc) and code
;; improvements.
;;
;; Revision 1.34  2001/08/07 05:30:02  paulk
;; Addes jde-parse-select-qualified-class-name.
;;
;; Revision 1.33  2001/06/28 04:58:03  paulk
;; Fixed mode test.
;;
;; Revision 1.32  2001/06/12 07:18:55  paulk
;; Changed jde-parse-declared-type-of to return a qualified type.
;; Thanks to "Evan Easton" <evan@eeaston.com> .
;;
;; Revision 1.31  2001/05/31 05:14:39  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.30  2001/03/16 04:49:08  paulk
;; Use major-mode instead of mode-name for testing which mode buffer is in. Thanks to Kevin Burton burton@relativity.yi.org.
;;
;; Revision 1.29  2001/01/05 07:14:35  paulk
;; Removed old version of jde-parse-get-class-at-point.
;;
;; Revision 1.28  2001/01/03 06:10:48  paulk
;; Fixes infinite recursion bug in jde-parse-update-after-parse when creating a new file.
;;
;; Revision 1.27  2000/12/18 05:22:46  paulk
;; *** empty log message ***
;;
;; Revision 1.26  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.25  2000/10/25 04:44:23  paulk
;; Changed jde-auto-parse-disable-threshold to jde-auto-parse-max-buffer-size.
;;
;; Revision 1.24  2000/10/25 04:32:58  paulk
;; Moved code generated by the semantic bovinator to jde-java-grammar.el.
;;
;; Revision 1.23  2000/10/20 04:09:31  paulk
;; Now uses generalized version of classes menu shipped with
;; semantic. Thanks to David Ponce and Eric Ludlam.
;;
;; Revision 1.22  2000/10/08 12:55:39  paulk
;; *** empty log message ***
;;
;; Revision 1.21  2000/09/05 04:55:28  paulk
;; Bug fixes
;;
;; Revision 1.20  2000/08/31 05:31:15  paulk
;; * Now creates a binary tree, jde-parse-method-map, listing the
;; locations of all methods in the source buffer.
;;
;; * Now parses the source buffer 30 seconds after a change.
;;
;; Revision 1.19  2000/08/16 05:30:35  paulk
;; Set case-fold-search to nil to ensure case sensitivity when parsing buffer.
;;
;; Revision 1.18  2000/08/07 05:06:35  paulk
;; Fixes a couple of bugs in jde-parse-valid-declaration-at. Thanks to Lou Aloia <xlxa@rims.com> and Stephane <s.nicolas@videotron.ca> for the fixes.
;;
;; Revision 1.17  2000/07/28 06:27:46  paulk
;; Committing all modified files.
;;
;; Revision 1.16  2000/07/13 05:22:48  paulk
;; *** empty log message ***
;;
;; Revision 1.15  2000/06/29 02:33:42  paulk
;; Added sort option to Classes index menu. Thanks to David Ponce for this contribution.
;;
;; Revision 1.14  2000/06/22 03:40:16  paulk
;; Index menu now shows variable types and class definitions. Thanks to David Ponce for these enhancments. Changed the name of jde-enable-index-menu to jde-imenu-enable and jde-enable-full-method-signatures-index-menu to jde-imenu-include signature.
;;
;; Revision 1.13  2000/06/09 05:07:06  paulk
;; Classes index menu now shows full signatures of methods. Thanks to Ittay Freiman <ittay@vigiltech.com> for suggesting this enhancement and to David Ponce <david@dponce.com> for implementing it.
;;
;; Revision 1.12  2000/05/26 09:14:10  paulk
;; Updated grammar to handle argument variables with modifiers and array arguments.
;;
;; Revision 1.11  2000/05/16 04:08:55  paulk
;; Adds a Classes index menu to the Emacs menubar.
;;
;; Revision 1.10  2000/05/11 03:07:17  paulk
;; Updated bovinator grammar.
;;
;; Revision 1.9  2000/05/11 01:24:40  paulk
;; Added support for Eric Ludlam's semantic bovinator. Moved regular expression-based imenu indexer to this file.
;;
;; Revision 1.8  2000/03/16 05:18:11  paulk
;; Miscellaneous small bug fixes and enhancements.
;;
;; Revision 1.7  2000/03/08 03:47:02  paulk
;; Fixed regular expression in jde-parse-get-innermost-class-at-point to handle more cases. Thanks to Steve Haflich <smh@franz.com> for reporting the problem and providing a starting point for the fix.
;;
;; Revision 1.6  2000/02/16 04:40:33  paulk
;; Implemented Cygwin/XEmacs compatiblity fixes provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.5  2000/02/14 06:21:32  paulk
;; Fixed find innermost class regular expression.
;;
;; Revision 1.4  2000/02/09 05:18:10  paulk
;; Added methods for parsing symbol at point.
;;
;; Revision 1.3  2000/02/01 04:10:48  paulk
;; Fixed regular expression for classes to handle case where point is in
;; a constructor. Thanks to Francois Cogne <cogne@col.bsf.alcatel.fr>.
;;
;; Revision 1.2  1999/08/20 00:52:14  paulk
;; Added jde-parse-get-package-from-name function.
;;
;; Revision 1.1  1999/04/27 16:25:46  paulk
;; Initial revision
;;