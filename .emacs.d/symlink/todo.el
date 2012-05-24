(defvar symlink-todo-global-file "~/.todo"
  "Path to the todo file used by `symlink-todo-quick-jump' and friends.")

(defun symlink-todo-quick-enter ()
  "Prompts for a new todo item to be inserted into the global todo file."
  (interactive)
  (let ((item (read-string "TODO: ")))
    (if (string= "" item)
        (symlink-todo-quick-jump)
      (symlink-todo-add-global-item item))))

(defun symlink-todo-quick-jump ()
  "Visits the global todo file."
  (interactive)
  (find-file symlink-todo-global-file))

(defun symlink-todo-add-global-item (item)
  "Adds an item to the global todo file."
  (save-excursion
    (set-buffer (find-file-noselect symlink-todo-global-file))
    (when (not (= (point-min) (point-max)))
      (goto-char (point-max))
      (insert "\n"))
    (insert item)
    (symlink-todo-toggle))
  (message "TODO: Item added."))

(defun symlink-todo-toggle ()
  "Toggles the todo state if it's active, otherwise activates it. "
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (string= (char-to-string (char-after)) "[")
        (symlink-todo-toggle-status)
      (insert "[ ] "))
    (save-buffer)))

(defun symlink-todo-done? ()
  "Is this line a done todo item?"
  (save-excursion
    (move-beginning-of-line 1)
    (search-forward "[ ]" (+ 3 (point)) t)))

(defun symlink-todo-toggle-status ()
  "Toggle the todo state."
  (interactive)
  (save-excursion
    (if (symlink-todo-done?)
        (symlink-todo-set-done)
      (symlink-todo-set-begun))))

(defun symlink-todo-set-begun ()
  "Set a todo item as begun."
  (symlink-todo-set-status " "))

(defun symlink-todo-set-done ()
  "Set a todo item as done."
  (symlink-todo-set-status "X"))

(defun symlink-todo-set-status (status)
  "Give the current todo item to an arbitrary status."
  (save-excursion
    (move-beginning-of-line 1)
    (forward-char 1)
    (delete-char 1)
    (insert status)))

(defun symlink-todo-move-item-up ()
  "Moves the focused todo item down a line."  
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (when (not (= (point-min) (point)))
      (let ((word (delete-and-extract-region (point) (point-at-eol))))
        (delete-char 1)
        (forward-line -1)
        (insert (concat word "\n")))
      (save-buffer)))
  (when (not (= (point-min) (point)))
    (forward-line -2)))

(defun symlink-todo-move-item-down ()
  "Moves the focused todo item up a line."    
  (interactive)
  (let (eof chars)
    (setq chars (- (point) (point-at-bol)))
    (save-excursion
      (end-of-line 1)
      (setq eof (= (point-max) (point)))
      (when (not eof)
        (let ((word (delete-and-extract-region (point-at-bol) (point))))
          (delete-char 1)
          (forward-line 1)
          (insert (concat word "\n")))
        (save-buffer)))
    (when (not eof)
      (forward-line 1)
      
)))