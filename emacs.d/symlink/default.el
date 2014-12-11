; always use spaces, not tabs, when indenting
(setq indent-tabs-mode nil)

;Searches should ignore case. Be case *in*sensitive. Got it?
(setq case-fold-search t)

;Replace should preserve case in replacements
(setq case-replace t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;(require 'highlight-current-line)
;(highlight-current-line-on)


;(find-file "~/.emacs")

; ignore case when searching
(setq case-fold-search t)

; require final newlines in files when they are saved
(setq require-final-newline t)

;Searches should ignore case. Be case *in*sensitive. Got it?
(setq case-fold-search t)

;Replace should preserve case in replacements
(setq case-replace t)

; don't show the startup screen
(setq inhibit-startup-screen t)

; number of characters until the fill column
(setq fill-column 70)

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
