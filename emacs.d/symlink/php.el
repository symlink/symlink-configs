;; PHP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)

(defun php-mode-hook ()
  (setq tab-width 4)
  (c-basic-offset 4)
  (c-set-style "java")
  (c-hanging-comment-ender-p nil)
  (indent-tabs-mode nil)
  (c-tab-always-indent 0)

  (not
   (and (string-match "/\\(PEAR\\|pear\\)/" (buffer-file-name))
        (string-match "\.php$" (buffer-file-name))
        (string-match "\.module$" (buffer-file-name))
        (string-match "\.inc$" (buffer-file-name))
        (string-match "\.class$" (buffer-file-name))
        )))
