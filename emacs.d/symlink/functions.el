;; Toggle between PHP & HTML Helper mode.  Useful when working on
;; php files, that can been intertwined with HTML code
(defun toggle-php-html-mode ()
  (interactive)
  "Toggle mode between PHP & HTML Helper modes"
  (cond ((string= mode-name "HTML helper")
         (php-mode))
        ((string= mode-name "PHP")
         (html-helper-mode))))

(defun dos-to-unix ()
  "Convert a DOS buffer to Unix format."
  (interactive)
  (beginning-of-buffer)
  (replace-string "\r\n" "\n"))

(defun my-untabify ()
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "[ \t]+$" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	(goto-char (point-min))
	(if (search-forward "\t" nil t)
		(untabify (1- (point)) (point-max))))
  nil)

(defun bash ()
  (interactive)
  (let ((binary-process-input t)
        (binary-process-output nil))
    (shell)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))

;; insert date into buffer at point
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

(defun air ()
  (interactive)
  (replace-string "240" "180")
  (save-buffer))

(defun big ()
  (interactive)
  (replace-string "180" "240")
  (save-buffer))

(defun insert-soft-tab ()
  (interactive)
  (insert "  "))

(defun symlink-indent ()
  (interactive)
  (insert "  "))

(defun mustache ()
  (interactive)
  (tpl-mode)
  (setq truncate-lines t))

(defun email ()
  (interactive)
  (markdown-mode)
  (turn-on-word-wrap))

(defun symlink-zap-to-char (arg char)
  "Kill up to but excluding ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
This emulates Vim's `dt` behavior, which rocks."
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (- (point) 1)))
  (backward-char 1))

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun symlink-ido-find-config ()
  (interactive)
  (find-file
   (concat "~/.emacs.d/symlink/"
           (ido-completing-read "Config file: "
                                (directory-files "~/.emacs.d/symlink/"
                                                 nil
                                                 "^[^.]")))))

(defun symlink-delete-till-end-of-buffer ()
  "Deletes all text from mark until `end-of-buffer'."
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (end-of-buffer)
      (delete-region beg (point)))))

(defun symlink-ido-find-project ()
  (interactive)
  (find-file
   (concat "~/Code/" (ido-completing-read "Project: "
                           (directory-files "~/Code/" nil "^[^.]")))))

(defun symlink-goto-config ()
  (interactive)
  (find-file "~/.emacs.d/symlink.el"))

;; fix kill-word
(defun symlink-kill-word (arg)
  "Special version of kill-word which swallows spaces separate from words"
  (interactive "p")

  (let ((whitespace-regexp "\\s-+"))
    (kill-region (point)
                 (cond
                  ((looking-at whitespace-regexp) (re-search-forward whitespace-regexp) (point))
                  ((looking-at "\n") (kill-line) (symlink-kill-word arg))
                  (t (forward-word arg) (point))))))

(defun symlink-backward-kill-word (arg)
  "Special version of backward-kill-word which swallows spaces separate from words"
  (interactive "p")
  (if (looking-back "\\s-+")
      (kill-region (point) (progn (re-search-backward "\\S-") (forward-char 1) (point)))
    (backward-kill-word arg)))

; set the mode based on the shebang;
; TODO: this sometimes breaks
(defun symlink-shebang-to-mode ()
  (interactive)
  (let*
      ((bang (buffer-substring (point-min) (prog2 (end-of-line) (point) (move-beginning-of-line 1))))
       (mode (progn
               (string-match "^#!.+[ /]\\(\\w+\\)$" bang)
               (match-string 1 bang)))
       (mode-fn (intern (concat mode "-mode"))))
    (when (functionp mode-fn)
      (funcall mode-fn))))
;(add-hook 'find-file-hook 'symlink-shebang-to-mode)

; duplicate the current line
(defun symlink-duplicate-line ()
  (interactive)
    (beginning-of-line)
    (copy-region-as-kill (point) (progn (end-of-line) (point)))
    (textmate-next-line)
    (yank)
    (beginning-of-line)
    (indent-according-to-mode))

; for loading libraries in from the vendor directory
(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/Projects/symlink-configs/emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (symlink (concat "~/Projects/symlink-configs/emacs.d/symlink/" file)))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))
    (when (file-exists-p (concat symlink ".el"))
      (load symlink))))

(defun symlink-backward-kill-line ()
  (interactive)
  (kill-line 0))

(require 'thingatpt)
(defun symlink-change-num-at-point (fn)
  (let* ((num (string-to-number (thing-at-point 'word)))
         (bounds (bounds-of-thing-at-point 'word)))
    (save-excursion
      (goto-char (car bounds))
      (symlink-kill-word 1)
      (insert (number-to-string (funcall fn num 1))))))

(defun symlink-inc-num-at-point ()
  (interactive)
  (symlink-change-num-at-point '+))

(defun symlink-dec-num-at-point ()
  (interactive)
  (symlink-change-num-at-point '-))

(defun url-fetch-into-buffer (url)
  (interactive "sURL:")
  (insert (concat "\n\n" ";; " url "\n"))
  (insert (url-fetch-to-string url)))

(defun url-fetch-to-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (beginning-of-buffer)
    (search-forward-regexp "\n\n")
    (delete-region (point-min) (point))
    (buffer-string)))

(defun gist-buffer-confirm (&optional private)
  (interactive "P")
  (when (yes-or-no-p "Are you sure you want to Gist this buffer? ")
    (gist-region-or-buffer private)))

  (defun symlink-clean-slate ()
    "Kills all buffers except *scratch*"
    (interactive)
    (let ((buffers (buffer-list)) (safe '("*scratch*")))
      (while buffers
        (when (not (member (car buffers) safe))
          (kill-buffer (car buffers))
          (setq buffers (cdr buffers))))))


  (defun symlink/c-electric-brace (arg)
    "Inserts a closing curly, too."
    (interactive "*P")
    (c-electric-brace arg)
    (save-excursion
      (insert "\n")
      (insert "}")
      (indent-according-to-mode)))

;; from http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))



(defun list-colors-display-htm (&optional list)
  "Create HTML page which lists all the defined colors."
  (interactive)
  (if (and (null list) window-system)
      (progn
        (setq list (x-defined-colors))
        ;; Delete duplicate colors.
        (let ((l list))
          (while (cdr l)
            (if (facemenu-color-equal (car l) (car (cdr l)))
                (setcdr l (cdr (cdr l)))
              (setq l (cdr l)))))))
  (with-output-to-temp-buffer "*Colors*"
    (save-excursion
      (set-buffer standard-output)
      (insert "<html>\n"
              "<head>\n"
              "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">\n"
              "<title>Colors</title>\n"
              "</head>\n"
              "<body>\n"
              "<h1>Colors</h1>\n"
              "<p>\n"
              "<pre>\n")
      (let (s)
        (while list
          (insert (format (concat "<span style=\"background-color:%s\">%-20s</span>"
                                  "  "
                                  "<span style=\"color:%s\">%s</span>"
                                  "\n")
                  (html-color (car list)) (car list)
                  (html-color (car list)) (car list)))
          (setq list (cdr list))))
      (insert "</pre>"
              "</body>"
              "</html>"))))

(defun html-color (string)
  "Convert colors names to rgb(n1,n2,n3) strings."
  (format "rgb(%d,%d,%d)"
          (/ (nth 0 (x-color-values string)) 256)
          (/ (nth 1 (x-color-values string)) 256)
          (/ (nth 2 (x-color-values string)) 256)))
