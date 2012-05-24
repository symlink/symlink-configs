(if (eq window-system 'mac)
    (progn
	  (set-default-font "-apple-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-iso10646-1")))

(setq inhibit-splash-screen t)
(setq tags-file-name "TAGS")
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq mac-emulate-three-button-mouse nil)
(setq cua-highlight-region-shift-only t)
(prefer-coding-system 'utf-8)
(setq vc-handled-backends nil)
(cua-mode nil)
(setq gist-view-gist t)


(setq indent-tabs-mode nil)
; mac hiding
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq x-select-enable-clipboard 't)

;; MISC

;(scroll-bar-mode)
(setq tool-bar-mode nil)
(display-time)
(visual-line-mode)
