;; others
(load "symlink/dired")
(load "symlink/php")
(load "symlink/shell")
(load "symlink/javascript")
(load "symlink/template")
(load "symlink/highlight")
(load "symlink/python")
(load "symlink/sgml")
(load "symlink/markdown")
(load "symlink/css")
(load "symlink/tabbar")

;(load "symlink/erlang")
;(load "symlink/erc")
;(load "symlink/artist")
;(load "symlink/coffee")
;(load "symlink/lua")
;(load "symlink/svn")
;(load "symlink/autopair")
;(load "symlink/smarty")


;; all modes
(add-hook 'before-save-hook 'delete-trailing-whitespace)
