(require 'shell)
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq explicit-shell-args '("-login" "-i"))
; this is a test
(setq shell-command-switch "-c")

;(global-set-key "\C-x\C-z" 'shell) ; shortcut for shell
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(eval-after-load 'shell
;  '(progn
;     (define-key shell-mode-map [up] 'comint-previous-input;)
;     (define-key shell-mode-map [down] 'comint-next-input)
;     (define-key shell-mode-map "\C-p" 'comint-previous-input)
;     (define-key shell-mode-map "\C-n" 'comint-next-input)))
