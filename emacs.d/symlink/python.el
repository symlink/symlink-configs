
(require 'ido)
(ido-mode t)

; python-mode
(setq py-install-directory "~/.emacs.d/vendor/python-mode.el-6.2.0")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)
;(load-file "~/.emacs.d/vendor/emacs-for-python/epy-init.el")


;(add-to-list 'load-path "~/.emacs.d/vendor/emacs-for-python/") ;; tell where to load the various files
;(require 'epy-setup)      ;; It will setup other loads, it is required!
;(require 'epy-python)     ;; If you want the python facilities [optional]
;(require 'epy-completion) ;; If you want the autocompletion settings [optional]
;(require 'epy-editing)    ;; For configurations related to editing [optional]
;(require 'epy-bindings)   ;; For my suggested keybindings [optional]
;(require 'epy-nose)       ;; For nose integration

;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode)
;                                   interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode!!!" t)

;(add-hook 'python-mode-hook
;          (lambda ()
;            (define-key python-mode-map "\C-m"
;              'python-reindent-then-newline-and-indent)
;             (add-hook 'local-write-file-hooks
;                        '(lambda()
;                           (save-excursion
;                             (untabify (point-min) (point-max))
;                             (delete-trailing-whitespace))))
;             (set (make-local-variable 'tab-width) 4)
;             (set (make-local-variable 'indent-tabs-mode) 'nil)))

;(defun python-reindent-then-newline-and-indent ()
;  "Reindents the current line then creates an indented newline."
;  (interactive "*")
;  (newline)
;  (save-excursion
;    (end-of-line 0)
;    (indent-according-to-mode)
;    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
;  (when (python-previous-line-is-comment)
;      (insert "# "))
;  (indent-according-to-mode))

;(defun python-previous-line-is-comment ()
;  "Returns `t' if the previous line is a Python comment."
;  (save-excursion
;    (forward-line -1)
;    (python-line-is-comment)))

;(defun python-line-is-comment ()
;  "Returns `t' if the current line is a Python comment."
;  (save-excursion
;    (beginning-of-line)
;    (search-forward "#" (point-at-eol) t)))
