;;; tex-site.el - Site specific variables.

;; Copyright (C) 1991, 2000, 2001 Kresten Krab Thorup 
;; Copyright (C) 1993, 1994, 1997, 1999 Per Abrahamsen 

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.dk>
;; Version: 10.0g
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains variables customized for the local site.

;; It also contains all necessary autoloads, so the user can simple
;; enable AUC TeX by putting (load "tex-site") in his .emacs file,
;; or the administrator can insert it in the site-start.el file.
;;
;; The ideal place for this file is in the `site-lisp' directory.

;;; Code:

(when (< emacs-major-version 20)
  (error "AUC TeX requires Emacs 20 or later"))

(defvar no-doc
  "This function is part of AUC TeX, but has not yet been loaded.
Full documentation will be available after autoloading the function."
  "Documentation for autoload functions.")

;;; Customization:
;;
;; Copy variables you need to change from the start of `tex.el' and
;; insert them here.

(defvar TeX-lisp-directory (concat teidir  "/elisp/tex/auctex")
  "*The directory where the AUC TeX lisp files are located.")

;;; Autoloads:

(add-to-list 'load-path TeX-lisp-directory)

;; This hook will store bibitems when you save a BibTeX buffer.
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
(autoload 'BibTeX-auto-store "latex" no-doc t)

(autoload 'tex-mode "tex" no-doc t)
(autoload 'plain-tex-mode "tex" no-doc t)
(autoload 'ams-tex-mode "tex" no-doc t)
(autoload 'TeX-auto-generate "tex" no-doc t)
(autoload 'TeX-auto-generate-global "tex" no-doc t)
(autoload 'TeX-insert-quote "tex" no-doc t)
(autoload 'TeX-submit-bug-report "tex" no-doc t)
(autoload 'japanese-plain-tex-mode "tex-jp" no-doc t)
(autoload 'japanese-latex-mode "tex-jp" no-doc t)
(autoload 'japanese-slitex-mode "tex-jp" no-doc t)
;;(autoload 'texinfo-mode "tex-info" no-doc t)
(autoload 'latex-mode "latex" no-doc t)

(when (memq system-type '(windows-nt))
(setq TeX-command-list
  (list (list "TeX" "tex -interaction=nonstopmode %t" 'TeX-run-TeX nil t)
	(list "LaTeX" "%l -interaction=nonstopmode %t" 'TeX-run-LaTeX nil t)
	(list "LaTeX PDF" "pdflatex -interaction=nonstopmode %t" 'TeX-run-LaTeX nil t)
	(list "View" "%v" 'TeX-run-discard nil nil)
	(list "Print" "gsview32 %f" 'TeX-run-command t nil)
	(list "File" "dvips %d -o %f " 'TeX-run-command t nil)
	(list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
	(list "Index" "makeindex %s" 'TeX-run-command nil t)
	(list "Check" "lacheck %s" 'TeX-run-compile nil t)
	(list "Other" "" 'TeX-run-command t t)))
(setq LaTeX-command-style '(("." "latex --src-specials")))
(setq TeX-view-style '(("^a5$" "windvi %d -paper a5")
		       ("^landscape$" "windvi %d -paper a4r -s 4")
		       ("^epsf$" "gsview32 %f")
		       ("." "windvi -single %d")))

)
(provide 'tex-site)

;;; tex-site.el ends here
