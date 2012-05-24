(setq highlight-tail-colors '(("black" . 0)
							  ("#787878" . 25)
							  ("black" . 66)))

(setq highlight-tail-steps 14
	  highlight-tail-timer 1)

(require 'highlight-tail)
(message "Highlight-tail loaded - now your Emacs will be even more sexy!")
(highlight-tail-mode)