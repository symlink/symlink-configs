; general
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-g" 'gist-buffer-confirm)
(global-set-key "\C-xg" 'magit-status)
(global-set-key "\M-i" 'insert-soft-tab)
(global-set-key "\M-z" 'symlink-zap-to-char)
(global-set-key "\C-xp" 'symlink-ido-find-project)
(global-set-key "\C-cp" 'symlink-ido-find-config)
(global-set-key "\C-cP" 'symlink-goto-config)
(global-set-key [C-return] 'symlink-duplicate-line)
(global-set-key "\C-x\C-g" 'github-ido-find-file)
;(global-set-key "\C-R" 'replace-string)
(global-set-key (kbd "C-S-N") 'word-count)
(global-set-key (kbd "A-F") 'ack)
(global-set-key "\M-g" 'goto-line)
; todo
(global-set-key [M-return] 'symlink-todo-toggle)
(global-set-key "\C-xt" 'symlink-todo-quick-enter)
(global-set-key [M-down] 'symlink-todo-move-item-down)
(global-set-key [M-up] 'symlink-todo-move-item-up)

; vim emulation
(global-set-key [C-tab] 'other-window)
;; (global-set-key [M-up] 'symlink-inc-num-at-point)
;; (global-set-key [M-down] 'symlink-dec-num-at-point)
(global-set-key (kbd "C-*") 'isearch-forward-at-point)
(global-set-key [remap kill-word] 'symlink-kill-word)
(global-set-key (kbd "C-S-k") 'symlink-backward-kill-line)
(global-set-key [remap backward-kill-word] 'symlink-backward-kill-word)
(global-set-key [remap aquamacs-backward-kill-word] 'symlink-backward-kill-word)

;; no printing!
;; no changing meta key!!
(when (boundp 'osx-key-mode-map)
  ;; Option is my meta key.
  (define-key osx-key-mode-map (kbd "A-;")
    '(lambda () (interactive) (message "noop")))

  ;; What's paper?
  (define-key osx-key-mode-map (kbd "A-p")
    '(lambda () (interactive) (message "noop"))))

; no mailing!
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")

(global-set-key [f1] 'python-mode)
(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'indent-region)
(global-set-key [f4] 'indent-buffer)
(global-set-key [f5] 'toggle-php-html-mode)
