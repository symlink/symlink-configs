;; basic loading for tei-emacs project
;; C Wittern/S Rahtz April 2001
;; Revised: $Date: 2004/03/25 $

(defmacro parent-directory (f)
  "Return safe parent directory of the directory given as argument."
  `(directory-file-name
    (file-name-directory
     (directory-file-name ,f))))

;; set up standard paths
(setq sitelispdir (concat teidir  "/elisp"))
(setq unicodelispdir (concat sitelispdir "/ucs"))
(setq generallispdir (concat sitelispdir "/general"))
(setq xmllispdir     (concat sitelispdir "/xml"))
(add-to-list 'load-path xmllispdir)
(add-to-list 'load-path (concat xmllispdir "/nxml-mode"))
(add-to-list 'load-path unicodelispdir)
(add-to-list 'load-path generallispdir)
(add-to-list 'load-path (concat sitelispdir "/tex/auctex"))
(add-to-list 'load-path (concat sitelispdir "/tex"))

;; Subversion
(load-library (concat sitelispdir "/psvn"))

(if (eq system-type 'windows-nt)    (load-library "windoze"))

;; general setup
(setq-default frame-title-format '("%f"))
(setq-default icon-title-format '("%f"))


;;This saves stuff between sessions
;(require 'session)
;(add-hook 'after-init-hook 'session-initialize)


(setq line-number-display-limit 100000)
(setq column-number-mode t)
(setq next-line-add-newlines nil)

;; timestamp

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-line-limit 20)

(autoload 'all "all" nil t)

(defun insert-date-time()
  (interactive)
  (insert (format-time-string "[%Y-%m-%dT%T%z]" (current-time))))


;; line number into modeline
(add-hook 'text-mode-hook '(lambda () (line-number-mode 1)))


;; wretched Windows leaves a / on the end if the root is "c:"
(if (string-match "/$" teidir) (setq teidir (substring teidir 0 -1)))

;; autofill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)

;;;  coloring syntax for LaTeX sources (and other languages)
(global-font-lock-mode t)
(add-hook 'help-mode-hook 'turn-on-font-lock)
(setq font-lock-maximum-decoration 3)

;; URL browsing
(require 'browse-url)
;; to let url loading work
;(setq browse-url-browser-function
;      ;; No window system at build time, ie.  site-start.el, ~/.emacs
;      ;; or ~/.gnus
;      (if window-system
;          (if (eq system-type 'windows-nt) 'shell-execute-url
;            'browse-url-mozilla)
;        'browse-url-w3))

;; AUC-TeX mode for LaTeX
(if window-system
      (require 'font-latex))
(require 'tex-site)
(require 'bib-cite)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook '(lambda () (local-unset-key "\M-g")))
(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key "\M-g" 'goto-line)))
(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key "\C-cg" 'LaTeX-fill-region)))
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)

(require 'func-doc)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "tp6")

(auto-compression-mode t)

;;; Cursor position, line number

(defun what-char-and-line ()
  "Print info on cursor position, line the point is on,
 and the total number of lines in the buffer."
  (interactive)
  (let* ((char (following-char))
         (beg (point-min))
         (end (point-max))
         (pos (point))
         (total (buffer-size))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
         (hscroll (if (= (window-hscroll) 0)
                      ""
                    (format " Hscroll=%d" (window-hscroll))))
         (col (current-column)))
    (if (= pos end)
        (if (or (/= beg 1) (/= end (1+ total)))
            (message "point=%d/%d(%d%%) <%d - %d>  col %d %s End of buffer. Last line %d."
                     pos total percent beg end col hscroll (count-lines 1 (point-max)))
          (message "point=%d/%d(%d%%)  col %d %s End of buffer. Last line %d."
                   pos total percent col hscroll (count-lines 1 (point-max))))
      (if (or (/= beg 1) (/= end (1+ total)))
          (message "Car= %s (0%o)  point=%d/%d(%d%%) <%d - %d>  col %d %s ligne %d/%d"
                   (single-key-description char) char pos total percent beg end col hscroll
                   (count-lines 1 (1+ (point))) (count-lines 1 (point-max)))
        (message "Car = %s (0%o)  point=%d/%d(%d%%)  col %d %s ligne %d/%d"
                 (single-key-description char) char pos total percent col hscroll
                 (count-lines 1 (1+ (point))) (count-lines 1 (point-max)))))))

;;; Postscript printing

(setq ps-paper-type 'a4)


;; Paren mode
(load "paren")
(defun kill-something( ) (interactive)
    (if (and mark-active transient-mark-mode)
        (kill-region (point) (mark)) 
        (delete-backward-char 1)
    ))

;;  JDE

;; Set the debug option to enable a backtrace when a
;; problem occurs.
;(setq debug-on-error t)

;; Update the Emacs load-path to include the path to
;; the JDE and its require packages. 
(add-to-list 'load-path (concat generallispdir "/jde/lisp"))
(add-to-list 'load-path (concat generallispdir "/eieio"))
(add-to-list 'load-path (concat generallispdir "/elib"))
(add-to-list 'load-path (concat generallispdir "/speedbar"))
(add-to-list 'load-path (concat generallispdir "/semantic"))

;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde nil)
;; to read:
;;
(setq defer-loading-jde t)
;;

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))


;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; Include the following only if you want to run
;; bash as your shell.

;; Setup Emacs to run bash as its primary shell.
;;(setq shell-file-name "bash")
;;(setq shell-command-switch "-c")
;;(setq explicit-shell-file-name shell-file-name)
;;(setenv "SHELL" shell-file-name)
;;(setq explicit-sh-args '("-login" "-i"))

;; CUA mode
(load "cua")
; setup at start
(load "rng-auto")
(load "xmlsgml-setup")
(if (memq window-system '(x w32))
 (progn
   (set-frame-height (selected-frame) 40)      
   (set-frame-width (selected-frame) 100)
   (require 'color-theme)
	))

;; character-related
(setq unicode-data-path (concat teidir "/unicode/UnicodeData-Latest.txt"))

;; define extra quail input method
(register-input-method
 "sanskrit-romanized-postfix" "Latin-1" 'quail-use-package
 "SK>" "Romanized Sanskrit postfix modifiers"
 "quail-sanskrit")

(if (> emacs-major-version 20)        
(progn
;; the unicode files are in /elisp/ucs
(add-to-list 'load-path (concat unicodelispdir "/Mule-UCS-current/lisp"))
(require 'un-define)
))
