
(add-to-list 'load-path "~/Projects/symlink-configs/emacs.d")
(add-to-list 'load-path "~/Projects/symlink-configs/emacs.d/vendor")
; custom place to save customizations
(setq custom-file "~/Projects/symlink-configs/emacs.d/symlink/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(when (file-exists-p ".passwords") (load ".passwords"))

(load "symlink/global")
(load "symlink/functions")
(load "symlink/modes")
(load "symlink/default")
