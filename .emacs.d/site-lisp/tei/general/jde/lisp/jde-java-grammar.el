;;; jde-java-grammar.el
;; $Revision: 1.7 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 2000, 2001 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'semantic-java)
(require 'jde-parse)
(eval-when-compile
  (require 'senator)
  (require 'jde-which-method)
  )

(defun jde-parse-semantic-default-setup ()
  "Setup the semantic bovinator for the JDE.
Should be run when Semantic is ready to parse that is via
`semantic-init-hooks'."
  
  ;; Remove me from `semantic-init-hooks'
  (remove-hook 'semantic-init-hooks 'jde-parse-semantic-default-setup)

  (when jde-auto-parse-enable
    (if (boundp 'global-semantic-auto-parse-mode)
        ;; Starting with version 1.4 beta 12 Semantic uses an idle timer
        ;; to automatically re-parse modified buffers if
        ;; `semantic-auto-parse-mode' minor mode is enabled.  If the
        ;; minor mode is enabled globally, that is if
        ;; `global-semantic-auto-parse-mode' is non-nil then just ignore
        ;; JDE's setting.
        (or global-semantic-auto-parse-mode
            (semantic-auto-parse-mode 1))
      ;; For Semantic versions prior to 1.4 beta 12 use JDE auto-parse
      ;; mechanism if enabled, based on buffer changes tracking.
      (make-local-hook 'semantic-change-hooks)
      (add-hook 'semantic-change-hooks
                #'jde-parse-buffer-changed-hook t t)))

  ;; Track full reparses
  (make-local-hook 'semantic-after-toplevel-cache-change-hook)
  (add-hook 'semantic-after-toplevel-cache-change-hook
	    #'jde-parse-update-after-parse nil t)

  ;; Track partial reparses
  (make-local-hook 'semantic-after-partial-cache-change-hook)
  (add-hook 'semantic-after-partial-cache-change-hook
	    #'jde-parse-update-after-partial-parse nil t)
  
  (when jde-enable-senator
    ;; Starting with version 1.4 beta 12 Semantic can globally enable
    ;; `senator-minor-mode'.  If so then just ignore JDE's setting.
    (or (and (boundp 'global-senator-minor-mode)
             global-senator-minor-mode)
        (senator-minor-mode 1)))

  ;; imenu & speedbar setup
  (jde-imenu-setup)

  (if (and jde-which-method-mode
           (not jde-which-method-idle-timer))
      (setq jde-which-method-idle-timer
            (run-with-idle-timer .25 t 'jde-which-method-update)))
    
  ;; initial parsing of the current buffer
  (semantic-bovinate-toplevel))

(provide 'jde-java-grammar)

;;; Change History:

;; $Log: jde-java-grammar.el,v $
;; Revision 1.7  2001/11/11 07:18:17  paulk
;; Moves all the `jde-mode' code depending on Semantic
;; into `jde-parse-semantic-default-setup' which is now run as a
;; `semantic-init-hooks' ensuring that at this point the buffer is ready
;; for parsing.
;;
;; This solves a nasty problem of synchronization between `jde-mode' and
;; Semantic initialization.  This problem was particularly annoying in
;; XEmacs where `jde-mode' buffers were not parsed the first time.  In
;; some rare conditions the problem occurred with Emacs too.
;;
;; Thanks to David Ponce.
;;
;; Revision 1.6  2001/11/08 06:16:03  paulk
;; Updated to support semantic 1.4 beta 12.
;;
;; Revision 1.5  2001/09/16 17:54:00  paulk
;; David Ponce moved all Semantic setup code from `jde-mode-internal' to
;; `jde-parse-semantic-default-setup' (which is called by
;; `jde-mode-internal') and added a hook to support partial re-parsing
;; of buffers.
;;
;; Revision 1.4  2001/05/19 02:35:59  paulk
;; Updated to support semantic 1.4. Thanks to David Ponce.
;;
;; Revision 1.3  2001/02/21 05:55:38  paulk
;; Added require for semantic package.
;;
;; Revision 1.2  2001/01/25 05:38:39  paulk
;; Changed the definition of formal_parameter_list to improve performance (less
;; backtracking) when parsing parameters in method and constructor
;; declarations. Thanks to David Ponce.
;;
;; Revision 1.1  2000/10/25 04:30:31  paulk
;; Initial revision.
;;

;; End of jde-java-grammar.el