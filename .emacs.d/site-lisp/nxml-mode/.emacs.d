;ELC   
;;; Compiled by jjc@pineapple.bkk.thaiopensource.com on Mon Oct  4 11:58:42 2004
;;; from file /home/jjc/elisp/rng-uri.el
;;; in Emacs version 21.3.1
;;; with bytecomp version 2.85.4.1
;;; with all optimizations.

;;; This file uses dynamic docstrings, first added in Emacs 19.29.
(if (and (boundp 'emacs-version)
	 (< (aref emacs-version (1- (length emacs-version))) ?A)
	 (or (and (boundp 'epoch::version) epoch::version)
	     (string-lessp emacs-version "19.29")))
    (error "`rng-uri.el' was compiled for Emacs 19.29 or later"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#@128 Return a URI for the filename F.
Multibyte characters are left as is. Use `rng-uri-escape-multibyte' to
escape them using %HH.
(defalias 'rng-file-name-uri #[(f) "\302!\303\304\305#\306	G\307V\203 	\307H\310U\203 \311\202 \312	Q)\207" [f url expand-file-name replace-regexp-in-string "[ -<>#%\"{}|\\^[]`%?;]" rng-percent-encode "file:" 0 47 "//" "///"] 4 (#$ . 614)])
#@37 Escape multibyte characters in URI.
(defalias 'rng-uri-escape-multibyte #[(uri) "\301\302\303\304\305\"#\207" [uri replace-regexp-in-string "[:nonascii:]" rng-percent-encode encode-coding-string utf-8] 6 (#$ . 997)])
(defalias 'rng-percent-encode #[(str) "\302\303\304\305\306	\307\")\"\"\207" [str string apply concat mapcar #[(ch) "\301\302\303\245\303\246#\207" [ch format "%%%x%x" 16] 5] string-to-sequence list] 7])
#@91 Return the filename represented by a URI.
Signal an error if URI is not a valid file URL.
(defalias 'rng-uri-file-name #[(uri) "\301\302\"\207" [uri rng-uri-file-name-1 nil] 3 (#$ . 1429)])
#@71 Return a regexp for filenames represented by URIs that match PATTERN.
(defalias 'rng-uri-pattern-file-name-regexp #[(pattern) "\301\302\"\207" [pattern rng-uri-file-name-1 match] 3 (#$ . 1626)])
(defalias 'rng-uri-pattern-file-name-replace-match #[(pattern) "\301\302\"\207" [pattern rng-uri-file-name-1 replace] 3])
(defalias 'rng-uri-file-name-1 #[(uri pattern) "\306\307\"\204\f \310\311\"\210\312!\313!\206 \310\314\"\211@	A@\315	8\306\316\f\"\317	8-\320	8.\n\204F /\204R \310\321\"\210\202R \n\227\322\230\204R \310\323\"\2100\324B\235\204` \310\325\"\210-\203j \310\326\"\210.\203t \310\327\"\210\306\330\f\"\203\200 \310\331\"\210\306\332\f\"\203\214 \310\333\"\2101\334=\203\245 \203\245 \335\f\336\337O!\203\245 \f\336\337O/\203\266 \306\340\f\"\203\266 \f\315\337O/\341=\203\303 \342\f!\202\324 /\343=\203\321 \344\f\315\"\202\324 \345\f!\306\346\f\"\203\341 \310\347\"\210/\341=\203\364 \203\360 \350\202\361 \351\fP/\341=\203\352\f\353Q\202/\343=\203\204\354\fP\202\f.\207" [uri components scheme authority path absolutep string-match "\\`\\(?:[^%]\\|%[0-9a-fA-F]{2}\\)*\\'" rng-uri-error "Bad escapes in URI `%s'" rng-uri-unescape-multibyte rng-uri-split "Cannot split URI `%s' into its components" 2 "\\`/" 3 4 "URI `%s' does not have a scheme" "file" "URI `%s' does not use the `file:' scheme" (nil "" "localhost") "URI `%s' does not start with `file:///' or `file://localhost/'" "`?' not escaped in file URI `%s'" "URI `%s' has a fragment identifier" ";" "`;' not escaped in URI `%s'" "%2[fF]" "Escaped slash in URI `%s'" windows-nt file-name-absolute-p 1 nil "\\`\\./" match rng-uri-unescape-unibyte-match replace rng-uri-unescape-unibyte-replace rng-uri-unescape-unibyte " " "URI `%s' has NUL character in path" "\\(\\)" "\\(\\(?:[^/]*/\\)*\\)" "\\`" "\\'" "\\1" query fragment-id pattern system-name system-type] 5])
(defalias 'rng-uri-error #[(&rest args) "\301\302\303\304\"C\"\207" [args signal rng-uri-error apply format] 5])
(byte-code "\300\301\302\303#\210\300\301\304\305#\207" [put rng-uri-error error-conditions (error rng-uri-error) error-message "Invalid URI"] 4)
(defalias 'rng-uri-split #[(str) "\301\302\"\205 \303\304\"\303\305\"\303\306\"\303\307\"\303\310\"\257\207" [str string-match "\\`\\(?:\\([^:/?#]+\\):\\)?\\(?://\\([^/?#]*\\)\\)?\\([^?#]*\\)\\(?:\\?\\([^#]*\\)\\)?\\(?:#\\(\\(?:.\\|\n\\)*\\)\\)?\\'" match-string 1 2 3 4 5] 7])
(defalias 'rng-uri-join #[(scheme authority path &optional query fragment-id) "\2055 \306\n\203 \307\nD\203 \310	BB	B\f\203& \311\f	BB\2030 \312	BB\313\314	\")\207" [path parts fragment-id query authority scheme nil "#" "?" "//" ":" apply concat] 3])
#@280 Resolve a possibly relative URI reference into absolute form.
URI-REF is the URI reference to be resolved.
BASE-URI is the base URI to use for resolving it.
The algorithm is specified by RFC 2396.
If there is some problem with URI-REF or BASE-URI, then
URI-REF will be returned.
(defalias 'rng-uri-resolve #[(uri-ref base-uri) "\306!\211@	A@\307	8\310	8\311	8\306!	\2031 \n\2041 \2031 @\2045 \202i @\204a A@\f\312\232\203Y \204Y \3078\3108\202a \313\f\3078\"\314\n\f%.\207" [uri-ref components scheme authority path query rng-uri-split 2 3 4 "" rng-resolve-path rng-uri-join fragment-id base-uri base-components] 7 (#$ . 4334)])
(defalias 'rng-resolve-path #[(path base-path) "\306\307\"\204 \306\307	\"\204 \207\310!\310	!\nG\311V\203( \312\n!\244\2020 \n@@P\240\210\313!\211@\314\232\203? \f\315\240\210)\316\314\"\317\211\317A\203\230 \204\230 \211A@)\320\232\203n @\320\232\203w A\211\202R \317\240\210A\211AA)?\205\214 \315\240\210\321\322\317\"\202P \204J *\323!*\207" [path base-path base-segments segments last-segment matched string-match "\\`/" rng-split-path 1 nbutlast last "." "" delete nil ".." t delq rng-join-path iter x] 4])
#@148 Return a URI that relative to BASE is equivalent to FULL.
The returned URI will be relative if possible.
Both FULL and BASE must be absolute URIs.
(defalias 'rng-relative-uri #[(full base) "\306!\211@	A@\307	8\310	8\311	8\306!	\203W \203W \n\203W \n@\232\203W \312\203L A@\232\203L \312\313\f\3078\"\314\n\f%\202X .\207" [full components scheme authority path query rng-uri-split 2 3 4 nil rng-relative-path rng-uri-join fragment-id base base-components] 7 (#$ . 5570)])
(defalias 'rng-relative-path #[(path base-path) "\304!\304	!\nG\305V\203 \306\n!\307\235\204+ \310\235\204+ \307\n\235\204+ \310\n\235\203/ \202d \203H \n\203H @\n@\230\203H A\nA\202/ \n\203V \nA\310B\202H @\311\232\203a \307B\312!*\207" [path base-path base-segments segments rng-split-path 1 nbutlast "." ".." "" rng-join-path] 3])
(defalias 'rng-split-path #[(path) "\303\304\305\306\n	#\203 \n	\303\224OB\303\225\202 \n	\304OB\237*\207" [segments start path 0 nil string-match "/"] 4])
(defalias 'rng-join-path #[(segments) "\205	 \301\302\303#\207" [segments mapconcat identity "/"] 4])
(defalias 'rng-uri-unescape-multibyte #[(str) "\301\302\303#\207" [str replace-regexp-in-string "\\(?:%[89a-fA-F][0-9a-fA-F]\\)+" rng-multibyte-percent-decode] 4])
(defalias 'rng-multibyte-percent-decode #[(str) "\301\302\303\304\305\306\307\"\"\"\310\"\207" [str decode-coding-string apply string mapcar #[(h) "\301\302\"\207" [h string-to-number 16] 3] split-string "%" utf-8] 8])
(defalias 'rng-uri-unescape-unibyte #[(str) "\301\302\303\304\211%\207" [str replace-regexp-in-string "%[0-7][0-9a-fA-F]" #[(h) "\301\302\303O\304\"\207" [h string-to-number 1 nil 16] 4] t] 6])
(defalias 'rng-uri-unescape-unibyte-match #[(str) "\301\302\303\304\211%\207" [str replace-regexp-in-string "%[0-7][0-9a-fA-F]\\|[^%]" #[(match) "\301\230\203 \302\207\303G\304U\203 \202 \305\304\306O\307\"!\207" [match "*" "\\([^/]*\\)" regexp-quote 1 string-to-number nil 16] 5] t] 6])
(defalias 'rng-uri-unescape-unibyte-replace #[(str next-match-index) "\301\302\303\304\211%\207" [str replace-regexp-in-string "%[0-7][0-9a-fA-F]\\|[^%]" #[(match) "\304\230\203 	\211T\305\306\n\")\207G\307U\203 \310H\202% \311\307\312O\313\"\211\314=\2033 \315\314\211\"\2026 \315!)\207" [match next-match-index n ch "*" format "\\%s" 1 0 string-to-number nil 16 92 string] 5] t] 6])
(provide 'rng-uri)
