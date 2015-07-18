(defun node-repl () (interactive)
       (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive"))
       (node-repl)
