;; jde-import.el --- Organize Java imports

;; Copyright (C) 2000, 2001, 2002 by David Ponce

;; Authors:     David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; Maintainers: David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; Created: 15 Nov 2000
;; Version: $Revision: 1.21 $
;; Keywords: java, tools
;; VC: $Id: jde-import.el,v 1.21 2002/09/06 13:07:12 jslopez Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This library adds commands to the JDE to insert and organize Java import
;; statements.

;;; Code:

(require 'efc)
(require 'semantic-util)

;;;;
;;;; Customization
;;;;

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defcustom jde-import-excluded-packages '("bsh.*")
  "*Specifies classes that should not be imported into a source file.
The value of this variable should be a regular expression. The
`jde-import-find-and-import' command does not import any classes whose
fully qualified names match the regular expression. If more than one
fully qualified class name matches the unqualified name that you specify,
the command prompts you to select only the classes that do not match the
regular expression."
  :group 'jde-project
  :type '(repeat (string :tag "Package")))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

;; auto sorting of import statements
(defcustom jde-import-auto-sort nil
  "*Automatically resort import statements after a `jde-import-import'.
If non-nil, the JDE automatically resorts the import statements when a new import statement is added using `jde-import-import' or `jde-import-find-and-import'."
  :group 'jde-project
  :type 'boolean
)

(defcustom jde-import-auto-sort-function 'jde-import-sort
  "*Function to call to automatically  sort imports statements after a `jde-import-import'.  
Usually `jde-import-sort' or `jde-import-organize'.  Enabled if  `jde-import-auto-sort' is not nil."
  :group 'jde-project
  :type 'function)

(defcustom jde-import-reverse-sort-group nil
  "*Non-nil to sort each import group's packages in reverse alphabetic
order.  See command `jde-import-organize'.  Note: For sorting the
groups, see variable `jde-import-sorted-groups'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-import-sorted-groups nil
  "*Non-nil to sort import groups in alphabetic order. Order may
be specified as alphabetic, reverse alphabetical or as implicitly 
specified by `jde-import-group-of-rules'. In the latter case the
order of groups is the same as their appearance in 
`jde-import-group-of-rules'.
See command `jde-import-organize'. Note: For sorting the packages
within each group, see variable `jde-import-reverse-sort-group'."
  :group 'jde-project
  :type '(choice :tag "Order"
                 (const :tag "No sort"                  nil)
                 (const :tag "group-of-rules order"     gor)
                 (const :tag "alphabetic order"         asc)
                 (const :tag "reverse alphabetic order" desc)))

(defcustom jde-import-group-function 'jde-import-group-of
  "*Function used to associate an import token to a group.
It receives one argument, the import token and must return a group
name string or nil if the import does not belong to any group.  The
function `jde-import-group-of' is the default value."
  :group 'jde-project
  :type 'function)

(defcustom jde-import-group-of-rules
  '(
    ("^javax?\\.")
    )
  "*Import group definitions used by `jde-import-group-of'.
Each group definition is a pair (REGEXP . GROUP) where:
- - REGEXP is a regexp that import names of this group must match.
- - GROUP is a group name or the index of the match data returned as
    group name or nil if REGEXP is the group name."
  :group 'jde-project
  :type '(repeat
          (cons :tag "Group Rule"
                regexp
                (choice :tag "Group Name"
                        (string  :tag "A String")
                        (integer :tag "Match data at")
                        (const   :tag "The Regexp" nil))))
  :set '(lambda (sym val)
          ;; delete empty entries!
          (set-default sym (delete '("") val))))

(defcustom jde-import-default-group-name nil
  "*Default group name if no group name is found.
If a group name is not found in `jde-import-group-of-rules' then this
group name is used.  If nil no default group name is used."
  :group 'jde-project
  :type '(choice (string  :tag "A String")
                 (const :tag "none" nil)))

(defcustom jde-import-insert-group-names nil
  "*If non-nil `jde-import-organize' inserts group name before imports.
See also the options `jde-import-group-of-rules' and
`jde-import-default-group-name'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-import-auto-collapse-imports nil
  "*If non-nil jde will automatically collapse imports when imports are
inserted."
  :group 'jde-project
  :type 'boolean)

(defun jde-import-get-imports()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))    
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports 
			     (list (buffer-substring-no-properties 
				    (match-beginning 1) 
				    (match-end 1)))))))
    imports))

(defun jde-import-get-import-insertion-point ()
  "Determine where to insert an import statement. 
If the buffer contains an import statement, return
the beginning of the next line; otherwise, if
the buffer contains a package statement, insert
three empty lines and return the beginning of
the second empty line; otherwise, if the buffer
contains a class definition, return the beginning
of the line before the class definition; otherwise,
return the beginning of the buffer."
  (let* ((tokens  (semantic-bovinate-toplevel t))
         (import-token
	  (car (last (semantic-find-nonterminal-by-token
                             'include tokens))))
         (package-token (car (semantic-find-nonterminal-by-token
                             'package tokens)))
         (class-token (car (semantic-find-nonterminal-by-token
                             'type tokens)))
        insertion-point)
    (cond (import-token
           (setq insertion-point (+ (semantic-token-end import-token) 1)))
          (package-token 
	   (save-excursion
	     (goto-char (semantic-token-end package-token))
             (forward-line)
	     (insert "\n")
             (setq insertion-point (point))
             (insert "\n\n")))
          (class-token
           (setq insertion-point (- (semantic-token-start class-token) 1)))
          (t 
           (setq insertion-point 1)))
    insertion-point))

(defun jde-import-import (class) 
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an import
statement for the specified class alrady exists."
  (interactive
   "sClass: ")
  (jde-import-insert-imports (list class)))

;; Contributed by David Ponce <david_ponce@mail.schneider.fr>
(defun jde-import-sort (&optional reverse)
  "Sort Java import statements alphabetically. In reverse order if
REVERSE is non-nil.

Usage:
  \\[jde-import-sort] sort import statements ascending.
  \\[universal-argument] \\[jde-import-sort] sort descending.

The the current buffer must be in `jde-mode'. This command uses the
semantic Java parser and requires JDE 2.1.6-beta24 and above."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found. Must be 'jde-mode'."))
  (or (and (local-variable-p 'semantic-toplevel-bovine-table (current-buffer))
           (symbol-value 'semantic-toplevel-bovine-table))
      (error "Semantic Java parser not found. JDE 2.1.6-beta24+ needed."))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq reverse t))
  (let* ((tokens  (semantic-bovinate-toplevel t))
         (depends (semantic-find-nonterminal-by-token 'include tokens)))
    (if depends
        (let* ((first-import-token (car depends))
               (last-import-token  (nth (1- (length depends)) depends))
               (start (semantic-token-start first-import-token))
               (end   (semantic-token-end   last-import-token)))
          (when (and start end)
	    (require 'sort)
	    (let (sort-fold-case)
                (sort-lines reverse start end)
                (goto-char start)))))))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-import-find-and-import (class)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can use
the variable `jde-import-excluded-packages' to prevent
specified classes from being imported or consider for import. This command uses
the JDE's BeanShell interpreter. It starts the interpreter if it is not
already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, except jars implicitly included by the jvm, e.g.,
rt.jar."
  (interactive
   (list (read-from-minibuffer "Class: "
			       (thing-at-point 'symbol))))
  (let (existing-import)
    (setq existing-import (jde-import-get-existing-import class))
    (if (not (null existing-import))
	(message "Skipping: already imported %s" existing-import)
      (let ((imports
             (bsh-eval-r
              (concat "jde.util.JdeUtilities.getQualifiedName(\""
                      class "\");"))))
        (setq imports (remove-duplicates imports :test 'equal))
        (if imports
            (jde-import-insert-imports imports)
          (message "Error: could not find %s." class))))))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-import-insert-imports (new-imports)
  (let* ((imports
	  (mapcar 'jde-import-strip-excluded-imports
	   (jde-import-strip-existing-imports new-imports 
					   (jde-import-get-imports)))))
    ;;Delete the nil which result from the excluded ones
    (setq imports (delq nil imports))
    (jde-import-choose-imports imports)))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

(defun jde-import-strip-excluded-imports (new-import)
  "Removes excluded imports from the list"
  ;; If the string matches the regexp, we want to ignore it.
  (if jde-import-excluded-packages
      (let (i n result)
        (setq i 0)
        (message "exclude-regexp=%s"
		 jde-import-excluded-packages)
        (setq n (length jde-import-excluded-packages))
        (setq result new-import)
        (while (< i n)
          (let ((exclude-regexp
                 (nth i jde-import-excluded-packages)))
            (message "exclude-regexp=%s" exclude-regexp)
            (message "new-import=%s" new-import)
	    (if (or (not (string-match "[.]" new-import))
		    (string-match exclude-regexp new-import))
                (progn
                  (message "Excluding import: %s" new-import)
                  (setq result nil)))
            (setq i (+ i 1))))
        result)
    new-import))

(defun jde-import-strip-excluded-import (exclude-regexp new-import)
  "Removes excluded imports from the list"
  ;;if the string matchs the regexp we want to ignore it.
  (if (string-match exclude-regexp (concat " " new-import))      
      (progn (message "Excluding import: %s" new-import)
             ())
    new-import))


(defun jde-import-insert-imports-into-buffer (new-imports)
  "Inserts imports into the correct place in the buffer."
  (save-excursion
    (goto-char (jde-import-get-import-insertion-point))
    (if (not jde-xemacsp) (deactivate-mark))
    (loop for new-import in new-imports do
 	  (progn
	    (insert
	     (concat "import " new-import ";\n"))
	    (message "Imported %s" new-import)))
    (if jde-import-auto-collapse-imports
	(let (jde-import-auto-collapse-imports) ;; setting this to avoid infinite recursion
	  (jde-import-collapse-imports)))
    (if jde-import-auto-sort
	(funcall jde-import-auto-sort-function))))


(defun jde-import-strip-existing-imports (new-imports existing-imports)
  "Exclude classes that have already been imported."
  (let (i n return-imports)
    (setq i 0)
    (setq n (length new-imports))
    (while (< i n)
      ;;iterate through the new imports
      (let((new-import
	    (nth i new-imports)))
	;;Strip out those alreay there
	(when (not (find new-import existing-imports :test 'string=))
	  (setq return-imports (nconc (list new-import)
				      return-imports))))
      (setq i(+ i 1)))
    ;;Return any that still exist
    return-imports))


;; begin JVL enhancement contributed by Jim LoVerde <loverde@str.com>
(defun jde-import-get-existing-import (class-name)
  ""
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil)
        (existing-import)
        (result nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
        (setq existing-import (buffer-substring-no-properties
                               (match-beginning 1)
                               (match-end 1)))
        (if (string-equal class-name
                          (jde-import-strip-package-from-class
			   existing-import))
            (setq result existing-import))))
    result))

(defun jde-import-already-imports-class (class-name)
  "Determine if a class is already being imported (ignoring packages)"
  (find class-name (jde-import-get-imports-no-package) :test 'string=))

(defun jde-import-strip-package-from-class (class-name)
  "Strips the package name from fully qualified java class"
  (let (i return-name)
    (setq return-name class-name)
    (setq i (string-match "[^.]*$" class-name))
    (if i
        (setq return-name (substring class-name i)))
    return-name))

(defun jde-import-get-imports-no-package()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports
			     (list (jde-import-strip-package-from-class
				    (buffer-substring-no-properties
				     (match-beginning 1)
				     (match-end 1))))))))
    imports))
;; end JVL enhancement contributed by Jim LoVerde <loverde@str.com>

(defun jde-import-choose-imports (new-imports)
  "Prompts the user to select a class to import from a list of similarly
 named candidates."
  (if (> (length new-imports) 1)
      (let ((dialog
             (efc-option-dialog
              "Classes name dialog"
              :options new-imports
              :text "Select import to insert.")))
        (efc-dialog-show dialog)
	(if (oref dialog selection)
	    (jde-import-insert-imports-into-buffer (list (oref dialog selection)))))
    (jde-import-insert-imports-into-buffer new-imports)))

;; Contributed by David Ponce.
(defun jde-import-kill-extra-imports (&optional comment)
  "Delete extra Java import statements.
An import statement is considered extra if it is a duplicate,
imports a class from the package to which this file belongs,
it is not referenced in the file, 
or imports a class belonging to an already imported package, i.e.,
a package already imported by an import statement ending in .*.
If optional argument COMMENT is non-nil, the extra import statements
are commented out instead of deleted. 

Usage:
  \\[jde-import-kill-extra-imports]
  to kills extra imports.
  \\[universal-argument] \\[jde-import-kill-extra-imports]
  to comment out extra imports.

The current buffer must be in `jde-mode'."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq comment t))
  (let* ((tokens   (semantic-bovinate-toplevel t))
         (imports  (semantic-find-nonterminal-by-token 'include tokens)))
    (if (not imports)
        (message "No import found")
      (let* ((packages (semantic-find-nonterminal-by-token 'package tokens))
	     (package-imports
	      (append
	       (mapcar 
		     (lambda (package)
		       ;; Return a global import name from PACKAGE token.
		       ;; That is add ".*" at end of token name.
		       (concat (semantic-token-name package) ".*")) 
		     packages)
                    (delq nil 
			  (mapcar 
			   (lambda (import)
			     ;; Return token name if IMPORT is global or nil if not.
			     ;; IMPORT is global if its name ends with ".*".
			     (let ((name (semantic-token-name import)))
			       (and (string-match "[.][*]\\'" name)
				    name)))
			   imports))))
	     (first-import (car imports))
	     extra-imports 
	     required-imports)
	;; Get the list of extra imports
	;; Going to character zero so the the count-matches method work.
	(goto-char 0)
      (while imports
	(let* ((import (car imports))
	       (name (semantic-token-name import))
	       (classname (jde-import-get-classname name))
               (case-fold-search nil)
               (number-of-matches 
		(substring (count-matches 
			    (concat "\\b" classname "\\b")) 0 2)))
	  (if (or 
	       ;; If name is already listed in the set
	       ;; of required imports...
	       (member name required-imports)
	       ;;or the class is not reference in the file
	       ;;and is not an import of the whole package i.e. .*
	       (and (< (string-to-number number-of-matches) 2)
		    (not (string= classname "*")))
	       ;; or imports a class in the current package...
	       (and
		;; make sure name is not a package import, e.g., foo.bar.*
		(not (string-match "[.][*]\\'" name))
		(member 
		 ;; convert class import to equivalent package import
                 ;; e.g., foo.barClass to foo.*
		 (concat
		  (substring 
		   name 
		   0  (or (string-match "[.][^.]+\\'" name)
			  (length name)))
		  ".*")
		 package-imports)))
	      ;; add name to the list of extra imports...
	      (setq extra-imports (cons import extra-imports))
	    ;; otherwise add to the list or required  imports
	    (setq required-imports (cons name required-imports))))
	(setq imports (cdr imports)))
      (if (not extra-imports)
	  (message "No extra imports found")
	(let ((count 0))
	  ;; Move the point at the beginning of the first import
	  (goto-char (semantic-token-start first-import))
	  (save-excursion
          ;; Kill or comment out extra imports
          (while extra-imports
	    (let* ((extra-import (car extra-imports))
		   (start (semantic-token-start extra-import))
		   (end (semantic-token-end extra-import)))
	      (setq count  (1+ count))
	      (if comment
		  (comment-region start end)
		;; The following assumes that there is only one import
		;; statement on the same line. Line end comments are deleted
		;; too.
		(kill-region start
			     (progn
			       (goto-char end)
			       (forward-line)
			       (point))))
	      (setq extra-imports (cdr extra-imports))))
	  (message "%d extra import%s removed"
                count (if (= count 1) "" "s")))))))))

;;;;
;;;; Helper functions
;;;;

(defun jde-import-get-classname(import)
  "Takes as an argument an import i.e. java.util.Vector.
And returns the class name. In the above example it will
return Vector"
  (let ((pieces (split-string import "\\.")) class)
    (setq class (car (last pieces)))
    (setq pieces (split-string class "\\$")) 
    (setq class (car (last pieces)))
    class))

(defun jde-import-group-of (import-token)
  "Return the group IMPORT-TOKEN belongs to or nil if not found.
A group is found as soon as the import name matches a regexp in
`jde-import-group-of-rules'.  The returned group name depends on the
corresponding group definition in `jde-import-group-of-rules'."
  (let ((import-name (semantic-token-name import-token))
        (groups      jde-import-group-of-rules)
        match rule regexp group)
    (while (and groups (not match))
      (setq rule    (car groups)
            groups  (cdr groups)
            regexp  (car rule)
            group   (cdr rule)
            match   (and (string-match regexp import-name)
                         (cond ((stringp  group)
                                group)
                               ((integerp group)
                                (match-string group import-name))
                               (t
                                regexp)))))
    match))

(defun jde-import-bucketize (imports)
  "Bucketize IMPORTS tokens.
Return a vector of buckets.  Each bucket is sorted alphabetically by
import name or in reverse order if `jde-import-reverse-sort-group' is
non-nil.  There is a bucket for each different group the function
specified by `jde-import-group-function' returns.  The last extra
bucket contains imports that do not belong to any group."
  (let (import group others bins bin i n)
    ;; Sort imports into an alist of groups.  Build a separate list
    ;; for imports not in any group.
    (while imports
      (setq import  (car imports)
            imports (cdr imports)
            group   (funcall (or jde-import-group-function
                                 #'jde-import-group-of)
                             import))
      (if (not group)
          (setq others (cons import others))
        (setq bin (assoc group bins))
        (if bin
            (setcdr bin (cons import (cdr bin)))
          (setq bins (cons (cons group (list import)) bins)))))
    ;; If required sort the bins by group name
    ;; Remember that bins are in reverse order at this point.
    (cond ((eq jde-import-sorted-groups 'asc)
           (setq bins (sort bins
                            (function
                             (lambda (bin1 bin2)
                               (string-lessp (car bin2)
                                             (car bin1)))))))
          ((eq jde-import-sorted-groups 'desc)
           (setq bins (sort bins
                            (function
                             (lambda (bin1 bin2)
                               (string-lessp (car bin1)
                                             (car bin2)))))))
	  ((eq jde-import-sorted-groups 'gor)
	   (let* ((group-list (mapcar (function
				       (lambda (item) (cdr item)))
				      jde-import-group-of-rules)))
	     (setq bins 
		   (sort bins 
			 (function
			  (lambda (bin1 bin2)
			    (let* ((name1 (car bin1))
				   (name2 (car bin2))
				   (idx1 (length (member name1 group-list)))
				   (idx2 (length (member name2 group-list))))
			      (< idx1 idx2)))))))))
    ;; Build the vector of buckets.
    (setq bins (vconcat
                (delq nil
                      (nreverse
                       (cons (cons jde-import-default-group-name
                                   others)
                             bins))))
          n    (length bins)
          i    0)
    ;; Sort each bucket.
    (while (< i n)
      (setq bin (aref bins i))
      (setcdr bin (if jde-import-reverse-sort-group
                      (semantic-sort-tokens-by-name-decreasing (cdr bin))
                    (semantic-sort-tokens-by-name-increasing (cdr bin))))
      (setq i (1+ i)))
    bins))

(defun jde-import-insert-group (group &optional skip-line name)
  "Insert a GROUP of import texts in the current buffer.
If optional SKIP-LINE is non-nil skip a line before the group.
If optional NAME is non-nil add it as comment just before the group."
  (when group
    (if skip-line
        (newline 2))
    (when (and jde-import-insert-group-names name)
      (insert comment-start name)
      (newline))
    (insert (car group))
    (setq group (cdr group))
    (while group
      (newline)
      (insert (car group))
      (setq group (cdr group)))))

;;;;
;;;; Commands
;;;;

;;;###autoload
(defun jde-import-organize (&optional force)
  "Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jde-import-group-function'.  Groups are inserted in the order they
are found unless `jde-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jde-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jde-import-organize] group and sort import statements.
  \\[universal-argument] \\[jde-import-organize] to force reordering.

The current buffer must be in `jde-mode'.  This command requires a
version of the JDE with the semantic parser."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq force t))
  (let* ((tokens  (semantic-bovinate-toplevel t))
         (imports (semantic-find-nonterminal-by-token 'include tokens)))
    (if imports
        (let* ((bins (jde-import-bucketize imports))
               (n    (length bins))
               i l sl changed group bin)
          (if force
              (setq changed t)
            ;; Check if imports already ordered
            (setq sl (apply #'append (mapcar #'cdr bins))
                  l  imports)
            (while (and (not changed) l)
              (setq changed (not (string-equal
                                  (semantic-token-name (car l))
                                  (semantic-token-name (car sl))))
                    l  (cdr l)
                    sl (cdr sl))))
          (if (not changed)
              (message "Import statements already ordered")
            ;; Imports need to be reordered.
            ;; 1- Get ordered import texts
            (setq i 0)
            (while (< i n)
              (setq bin (aref bins i))
              (setcdr bin (mapcar (function
                             (lambda (import)
                               (buffer-substring-no-properties
                                (semantic-token-start import)
                                (progn
                                  (goto-char (semantic-token-end import))
                                  (end-of-line) ; keep any line comment
                                  (point)))))
                            (cdr bin)))
              (setq i (1+ i)))
            ;; 2- Keep the point at the beginning of the first import
            (goto-char (semantic-token-start (car imports)))
            ;; 3- Kill current imports
            (kill-region (point)
                         (progn
                           (goto-char (semantic-token-end
                                       (car (reverse imports))))
                           (end-of-line)
                           (point)))
            ;; 4- Insert ordered imports
            (save-excursion
              ;; Insert the first group found
              (setq i 0)
              (while (and (< i n) (not group))
                (setq group (aref bins i)
                      i     (1+ i)))
              (jde-import-insert-group (cdr group) nil (car group))
              ;; Insert the others with a blank line before each group
              (while (< i n)
                (setq group (aref bins i)
                      i (1+ i))
                (jde-import-insert-group (cdr group) 'skip-line (car group)))
                ))))))

(defcustom jde-import-collapse-imports-threshold 0
  "Threshold level used by 'jde-import-collapse-imports to decide when a
package star import is used instead of single imports. Setting the
threshold to 0 (default) causes the JDE to not collapse anything at
all."
  :group 'jde-project
  :type 'number)

(defun jde-import-collapse-imports (&optional comments)
"Function that collapse multiple class imports from the same package
into a single .* package import. Uses
'jde-import-collapse-imports-threshold to decide when a .* statement
is generated. Implemented by add the package statements and then
invoke 'jde-import-kill-extra-imports to clean up."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (let* ((tokens    (semantic-bovinate-toplevel t))
	 (imports   (semantic-find-nonterminal-by-token 'include tokens)))
    (if (<= jde-import-collapse-imports-threshold 0)
	(message "Collapse threshold set to zero. No collapsing will occur.")
    (if (not imports)
	(message "No import found")
      (let* ((package-buckets (jde-import-collapse-imports-bucketize imports))
	     (extra-imports   nil)
	     (required-imports nil)
	     (new-imports nil))
	(while package-buckets
	  (let*
	      ((bucket (car package-buckets)))
	    (if (>= (length bucket) jde-import-collapse-imports-threshold)
		(progn
		  (add-to-list 'extra-imports (cdr bucket))
		  ;; Add the collapsing package statement
		  (add-to-list 'new-imports (concat (car bucket) ".*")))
	      (add-to-list 'required-imports (cdr bucket))))
	  (setq package-buckets (cdr package-buckets)))
	(jde-import-insert-imports-into-buffer new-imports)
	(jde-import-kill-extra-imports comments))))))

(defun jde-import-collapse-imports-bucketize (imports)
  "Put all imports into a bucket named as the package they belong to."
  (let ((package-buckets))
    (while imports
      (let* ((import (car imports))
	     (name (semantic-token-name import))
	     (packagename (jde-parse-get-package-from-name name))
	     (packagebin))
	(setq packagebin (assoc packagename package-buckets))
	(if packagebin
	    (setcdr packagebin (cons import (cdr packagebin)))
	  (setq package-buckets (cons (cons packagename (list import)) package-buckets)))
	(setq imports (cdr imports))))
  package-buckets))

(provide 'jde-import)


;;; History:
;;
;; $Log: jde-import.el,v $
;; Revision 1.21  2002/09/06 13:07:12  jslopez
;; Fixes jde-import-get-classname to handle inner classes.
;;
;; Revision 1.20  2002/08/07 06:36:17  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.19  2002/05/31 18:55:08  mnl
;; Added sorting of import statements according to their occurance in the
;; group-of-rules.
;;
;; Revision 1.18  2002/05/30 04:43:31  paulk
;; Fixes bug in jde-import-get-import-insertion-point that
;; caused it to mishandle insertion of import statements
;; after package statements. Thanks to Phillip Lord <p.lord@russet.org.uk>.
;;
;; Revision 1.17  2002/02/03 07:01:39  paulk
;; Adds support for inserting group names before groups. Thanks to
;; Brian Paulsmeyer and David Ponce.
;;
;; Revision 1.16  2002/01/28 07:07:50  paulk
;; * Adds jde-import-auto-collapse-imports variable. Customizing this variable to a nonnil value
;;   causes the JDE to run jde-import-collapse-imports after importing a class. Thanks to
;;   "Max Rydahl Andersen" <max@eos.dk>.
;;
;; * Also cleaned up the code to jde-import-insert-imports-into-buffer.
;;
;; Revision 1.15  2002/01/27 04:09:57  paulk
;; Remove duplicates from import list. Thanks to Stephen Molitar.
;;
;; Revision 1.14  2002/01/06 06:56:09  paulk
;; jde-import-choose-imports now checks for the case where the user cancels
;; the class selection dialog.
;;
;; Revision 1.13  2001/12/08 13:55:29  jslopez
;; Updates the function jde-import-choose-imports to use
;; efc-option-dialog.
;;
;; Revision 1.12  2001/11/30 11:13:43  jslopez
;; Rolling back my previous changes to
;; jde-import-choose-imports, to fix regression bug.
;;
;; Revision 1.11  2001/11/28 22:50:24  jslopez
;; Fixes compilation messages.
;;
;; Revision 1.10  2001/10/19 08:57:27  paulk
;; Adds the customization variable jde-import-auto-sort-function.
;; This variable allows you to specify the function used by
;; the JDE to sort imports. Thanks to Hugh Emberson.
;;
;; Revision 1.9  2001/07/31 05:22:48  paulk
;; Adds jde-import-collapse-imports command. Thanks to Max Rydahl Andersen.
;;
;; Revision 1.8  2001/07/06 02:10:43  paulk
;; Bug fix in removing unneeded import statements.
;;
;; Revision 1.7  2001/06/07 03:35:01  paulk
;; Further fine-tuned import insertion point function.
;;
;; Revision 1.6  2001/06/06 05:19:28  paulk
;; Improved calculation of import insertion point.
;;
;; Revision 1.5  2001/04/27 01:33:42  paulk
;; jde-import-sort now refreshes parse cache. Thanks to Robert Mecklenburg <mecklen@cimsoft.com> for tthis fix.
;;
;; Revision 1.4  2001/04/26 09:06:07  paulk
;; -- jde-import-kill-extra-imports now refreshes the buffer's parse cache. This fixes a bug where successive calls to the function would incorrectly remove imports.
;;
;; -- jde-import-kill-extra-imports now removes fully qualified imports that are not referenced in the code.
;;
;; Thanks to "Javier Lopez" <jlopez@cellexchange.com>.
;;
;; Revision 1.3  2001/03/13 04:19:45  paulk
;; Cosmetic changes.
;;
;; Revision 1.2  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.1  2000/11/20 05:15:15  paulk
;; Added jde-import-organize command. Moved all import-related code from
;; jde-wiz.el to a new package named jde-import.el.
;;
;; Revision 1.2  2000/11/17 11:52:54  david_ponce
;; - New `jde-import-group-function' option to specify the function used
;;   to associate import token to group. The default one is
;;   `jde-import-group-of'. This let the user to completely handle the
;;   way imports are grouped.
;;
;; - New `jde-import-sorted-groups' option to specify if groups will be
;;   sorted. Notice that the *default* group (the one that contains
;;   imports not belonging to any specific group) is allways the last
;;   group.
;;
;; - Improvement of the function `jde-import-group-of'. For consistency
;;   `jde-import-group-rules' is now `jde-import-group-of-rules' and it
;;   is now possible to associate a group regexp to a particular name.
;;
;; Revision 1.1  2000/11/17 11:48:31  david_ponce
;; Initial Revision.
;;

;;; jde-import.el ends here
