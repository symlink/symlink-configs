;; others
(load "symlink/dired")
(load "symlink/php")
(load "symlink/shell")
(load "symlink/javascript")
(load "symlink/erlang")
(load "symlink/python")
(load "symlink/sgml")
(load "symlink/erc")
(load "symlink/artist")
(load "symlink/coffee")
(load "symlink/markdown")
(load "symlink/css")
(load "symlink/lua")
(load "symlink/svn")
(load "symlink/svn")
(load "symlink/template")
(load "symlink/theme")
(load "symlink/highlight")
(load "symlink/autopair")
(load "symlink/smarty")
(load "symlink/tabbar")

;; all modes
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; emacs
(define-key emacs-lisp-mode-map (kbd "A-r") 'eval-buffer)

; bash
(setq auto-mode-alist (cons '("\\.bashrc" . sh-mode) auto-mode-alist))

; obj-c
(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
;; (setq c-default-style "bsd" c-basic-offset 2)

; magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

; paredit
(autoload 'paredit-mode "paredit"
     "Minor mode for pseudo-structurally editing Lisp code."
     t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))

; jasper
(setq auto-mode-alist (cons '("\\.jr" . emacs-lisp-mode) auto-mode-alist))

; mode-compile
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
 "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


; c
;(add-hook 'c-mode-hook
;          '(lambda ()
;             (setq c-auto-newline t)
;             (define-key c-mode-map "{" 'symlink/c-electric-brace))
;          )



