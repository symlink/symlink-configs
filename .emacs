
; My stuff
(setq user-full-name "Bobby Gaza")

(if (eq window-system 'mac)
    (progn
	  (set-default-font "-apple-monaco-medium-r-normal--10-100-72-72-m-100-mac-roman")))

(setq mac-allow-anti-aliasing nil)

; mac hiding
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq x-select-enable-clipboard 't)

; Set some paths for loading...
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/site-lisp" ) load-path ))
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/mine" ) load-path ))
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/site-lisp/geben" ) load-path ))
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/site-lisp/geben/gud" ) load-path))
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/site-lisp/mmm-mode" ) load-path ))
(setq load-path (nconc '( "/Volumes/symlink/Documents/.emacs.d/site-lisp/nxml-mode" ) load-path ))

(add-to-list 'load-path
			 "/Volumes/symlink/Documents/.emacs.d/mine")

(add-to-list 'load-path "/Volumes/symlink/Documents/.emacs.d/site-lisp/eieio-0.17")
(load-file "/Volumes/symlink/Documents/.emacs.d/site-lisp/cedet-1.0pre4/common/cedet.el")

(autoload 'geben "geben" "PHP Debugger on Emacs" t)
(add-to-list 'load-path "/Volumes/symlink/Documents/.emacs.d/site-lisp/geben")
(add-to-list 'load-path "/Volumes/symlink/Documents/.emacs.d/site-lisp/geben/gud")

; custom stuff for ME
(load-file "/Volumes/symlink/Documents/.emacs.d/mine/my-color-theme.el")
(my-color-theme)

;;; some crazy highlighting!!
;(setq highlight-tail-colors '(("black" . 0)
;							  ("#787878" . 25)
;							  ("black" . 66)))

;(setq highlight-tail-steps 14
;	  highlight-tail-timer 1)

;(require 'highlight-tail)
;(message "Highlight-tail loaded - now your Emacs will be even more sexy!")
;(highlight-tail-mode)
;;

;; MISC
(tool-bar-mode)
(scroll-bar-mode)
;(setq tool-bar-mode nil)
(display-time)
(global-set-key "\M-g" 'goto-line)

;; WORK
(setq user-mail-address "bobby@thismoment.com")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/usr/local/mysql/bin")))
(setq exec-path (append exec-path '("/thismoment/apps/OSX/php/bin")))

(custom-set-variables
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "java") (php-mode . "java"))))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode t)
 '(php-mode-force-pear t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(transient-mark-mode t))

;; YASnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/Volumes/symlink/Documents/snippets/")

(autoload 'findr "findr" "Find file name." t)
(define-key global-map [(meta control S)] 'findr)

(autoload 'findr-search "findr" "Find text in files." t)
(define-key global-map [(meta control s)] 'findr-search)

(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map [(meta control r)] 'findr-query-replace)

; C-x r m set
; C-x r b jump
; bookmark list C-x r l
(global-set-key "\C-xs" 'bookmark-set)
(global-set-key "\C-xj" 'bookmark-jump)
(global-set-key "\C-xd" 'bookmark-delete)

; SVN
(define-key global-map [(meta s)] 'svn-status)
;(global-set-key [M-s] 'svn-status)
;(global-set-key [M-sd] 'svn-log-edit-done)


;;;;Enable the bell- but make it visible and not aural.
(setq visible-bell t)

;;;;Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;Fix the whole huge-jumps-scrolling-between-windows nastiness
(setq scroll-conservatively 5)

;;;;"Don't hscroll unless needed"- ? More voodoo lisp.
(setq hscroll-margin 1)

;;;;What it says. Keeps the cursor in the same relative row during
;;;;pgups and dwns.
(setq scroll-preserve-screen-position t)

 ;;;;;Accelerate the cursor when scrolling.
(load "accel" t t)

;;;;Start scrolling when 2 lines from top/bottom
(setq scroll-margin 2)

;;;;;Push the mouse out of the way when the cursor approaches.
(mouse-avoidance-mode 'jump)

;;;;Make cursor stay in the same column when scrolling using pgup/dn.
;;;;Previously pgup/dn clobbers column position, moving it to the
;;;;beginning of the line.
;;;;http://www.dotemacs.de/dotfiles/ElijahDaniel.emacs.html
(defadvice scroll-up (around ewd-scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))
                                
(defadvice scroll-down (around ewd-scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it                          
    (move-to-column col))) 


;;;SavePlace- this puts the cursor in the last place you editted
;;;a particular file. This is very useful for large files.
(require 'saveplace)
(setq-default save-place t)

;;;;Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;M-dn and M-up do nothing! :(  Let's make them do something, like M-
;;;left and M-right do.
(global-set-key [M-down] '(lambda () (interactive) (progn (forward-line 4) (recenter) ) ))
(global-set-key [M-up]   '(lambda () (interactive) (progn (forward-line -4) (recenter) ) ))

(defun dos-to-unix ()
  "Convert a DOS buffer to Unix format."
  (interactive)
  (beginning-of-buffer)
  (replace-string "\r\n" "\n"))

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq explicit-shell-args '("-login" "-i"))
(setq shell-command-switch "-c")

;Searches should ignore case. Be case *in*sensitive. Got it?
(setq case-fold-search t)

;Replace should preserve case in replacements
(setq case-replace t)

(defun bash ()
  (interactive)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (shell)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))

(global-set-key [f12] 'indent-buffer)
(global-set-key [f4] '(lambda () (interactive) (cvs-examine (file-name-directory (buffer-file-name)) '("-d" "-P"))))
(global-set-key [f5] '(lambda () (interactive) (cvs-status) '("-f" "-v")))
(global-set-key [f1] 'php-mode)
(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'indent-region)

(require 'tabbar)
(tabbar-mode)

;(let ((color-theme-is-global nil))
;  (select-frame (make-frame))
;  (color-theme-taming-mr-arneson))

(require 'highlight-current-line)
(highlight-current-line-on)

;; set the menu font
(let ((spec '((t (:family "clean" :size "12pt")))))
  (mapc (lambda (face)
          (face-spec-set face spec)
          (put face 'face-defface-spec spec))
        '(default menu)))

;; subversion ;;
(require 'psvn)

;; Template Features ;;
(require 'template)
(template-initialize)

;; GMAIL
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "bobby@symlinked.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "symlinked.com")
;; end GMAIL
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)

;; the below won't work until we get the w3 package
(load-file "/Volumes/symlink/Documents/.emacs.d/site-lisp/weblogger.el")

(defun weblog-filter-through-program (program)
  (save-excursion
    (goto-char (point-min))
    (search-forward "nn")
    (shell-command-on-region (point) (point-max) program t t)))
(defun weblog-entry-text-to-html ()
  (interactive)
  (weblog-filter-through-program "text2html"))
(defun weblog-entry-html-to-text ()
  (interactive)
  (weblog-filter-through-program "html2text"))
(defun my-weblog-mode-hook ()
  (local-set-key "C-cbh" 'weblog-entry-text-to-html)
  (local-set-key "C-cbt" 'weblog-entry-html-to-text))

;; RPM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
			      auto-mode-alist))

;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; PHP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)

(defun php-mode-hook ()
  (setq tab-width 4)
  (c-basic-offset 4)
  (c-set-style "java")
  (c-hanging-comment-ender-p nil)
  (indent-tabs-mode t)
  (c-tab-always-indent 0)

  (not
   (and (string-match "/\\(PEAR\\|pear\\)/" (buffer-file-name))
        (string-match "\.php$" (buffer-file-name))
        (string-match "\.module$" (buffer-file-name))
        (string-match "\.inc$" (buffer-file-name))
        (string-match "\.class$" (buffer-file-name))
        )))

(defun my-untabify ()
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "[ \t]+$" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	(goto-char (point-min))
	(if (search-forward "\t" nil t)
		(untabify (1- (point)) (point-max))))
  nil)

;(add-hook 'php-mode-hook
;		  (lambda()
;			(make-local-variable 'write-contents-hooks)
;			(add-hook 'write-contents-hooks 'php-untabify)))

;; PHP/HTML MIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "/Volumes/symlink/Documents/.emacs.d/site-lisp/nxml/autostart.el");
(require 'nxml-mode)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-group
 'fancy-html
 '(
   (html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]"
    :back "[?]>")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "style=\""
    :back "\"")))

;; What files to invoke the new html-mode for?
;(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
;(add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php\\'" . html-mode))
;(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
;(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
;;
;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
;; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
(defun go-bind-markup-menu-to-mouse3 ()
  (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
;;
(add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

;; CSS/HTML/XML/Smarty ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "/Volumes/symlink/Documents/.emacs.d/site-lisp/nxml-mode/rng-auto.el")
(load-file "/Volumes/symlink/Documents/.emacs.d/site-lisp/css-mode.el")
(require 'smarty-mode)
(add-hook 'smarty-mode-user-hook 'turn-on-font-lock)

(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-line-function 'indent-relative)))

(setq auto-mode-alist
      (append
       (list
	'("\\.sgm" . docbook-mode)
	'("\\.sgml" . docbook-mode)
	'("\\.css" . css-mode)
	'("\\.xml" . nxml-mode)
	'("\\.html" . html-mode)
	'("\\.ihtml" . html-mode)
	'("\\.tmpl" . html-mode)
	'("\\.xtmpl" . html-mode))
       auto-mode-alist))
;	'("\\.tpl" . nxml-mode)
;; insert date into buffer at point
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))


(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Backups
;;;;Change backup behavior to save in a directory, not in a miscellany
;;;;of files all over the place.
;(setq
; backup-by-copying t      ; don't clobber symlinks
; backup-directory-alist
; '(("." . "/Volumes/symlink/Documents/backups"))    ; don't litter my fs tree
; delete-old-versions t
; kept-new-versions 6
; kept-old-versions 2
; version-control t)       ; use versioned backups
;
;(setq backup-by-copying t backup-directory-alist '(("." . "/Volumes/symlink/Documents/backups"))
;      delete-old-versions t
;      kept-new-versions 6
;      kept-old-versions 2
;      version-control t)     
;(setq backup-directory-alist `(("." . ,(expand-file-name "/Volumes/symlink/Documents/backups"))))
