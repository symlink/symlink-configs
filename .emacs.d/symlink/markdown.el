(add-to-list 'load-path "~/.emacs.d/vendor/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ronn?" . markdown-mode) auto-mode-alist))

(add-hook
 'markdown-mode-hook
 '(lambda ()
    (define-key markdown-mode-map (kbd "C-c i") 'symlink-markdown-insert-link)
    (define-key markdown-mode-map (kbd "A-r") 'markdown-preview)
    (define-key markdown-mode-map (kbd "<tab>") 'symlink-indent)))
