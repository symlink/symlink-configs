(require 'iso-insert)

(defun alt-iso () "alt-iso"
  (global-set-key [?\A-|] 'insert-u-umlaut)
  (global-set-key [?\A-!] 'insert-inverted-exclamation-mark)
  (global-set-key [?\A-@] 'insert-A-grave)
  (global-set-key [?\A-#] 'insert-pound-sign)
  (global-set-key [?\A-$] 'insert-general-currency-sign)
  (global-set-key [?\A-%] 'insert-yen-sign)
  (global-set-key [?\A-^] 'insert-THORN)
  (global-set-key [?\A-&] 'insert-broken-vertical-line)
  (global-set-key [?\A-*] 'insert-ordinal-indicator-feminine)
  (global-set-key [?\A-(] 'insert-diaeresis)
  (global-set-key [?\A-)] 'insert-copyright-sign)
  (global-set-key [?\A-_] 'insert-ss)
  (global-set-key [?\A-+] 'insert-angle-quotation-mark-left)
  (global-set-key [?\A-Q] 'insert-N-tilde)
  (global-set-key [?\A-W] 'insert-multiplication-sign)
  (global-set-key [?\A-E] 'insert-A-ring)
  (global-set-key [?\A-R] 'insert-O-grave)
  (global-set-key [?\A-T] 'insert-O-circumflex)
  (global-set-key [?\A-Y] 'insert-U-grave)
  (global-set-key [?\A-U] 'insert-O-tilde)
  (global-set-key [?\A-I] 'insert-E-acute)
  (global-set-key [?\A-O] 'insert-I-umlaut)
  (global-set-key [?\A-P] 'insert-D-stroke)
  (global-set-key [?\A-{] 'insert-u-circumflex)
  (global-set-key [?\A-}] 'insert-y-acute)
  (global-set-key [?\A-A] 'insert-A-acute)
  (global-set-key [?\A-S] 'insert-O-acute)
  (global-set-key [?\A-D] 'insert-A-umlaut)
  (global-set-key [?\A-F] 'insert-AE)
  (global-set-key [?\A-G] 'insert-C-cedilla)
  (global-set-key [?\A-H] 'insert-E-grave)
  (global-set-key [?\A-J] 'insert-E-circumflex)
  (global-set-key [?\A-K] 'insert-E-umlaut)
  (global-set-key [?\A-L] 'insert-I-grave)
  (global-set-key [?\A-:] 'insert-degree-sign)
  (global-set-key [?\A-"] 'insert-cent-sign)
  (global-set-key [?\A-~] 'insert-thorn)
  (global-set-key [?\A-Z] 'insert-U-acute)
  (global-set-key [?\A-X] 'insert-O-slash)
  (global-set-key [?\A-C] 'insert-A-tilde)
  (global-set-key [?\A-V] 'insert-O-umlaut)
  (global-set-key [?\A-B] 'insert-A-circumflex)
  (global-set-key [?\A-N] 'insert-I-circumflex)
  (global-set-key [?\A-M] 'insert-I-acute)
  (global-set-key [?\A-<] 'insert-fraction-one-quarter)
  (global-set-key [?\A->] 'insert-fraction-three-quarters)
  (global-set-key [?\A-?] 'insert-inverted-question-mark)
  (global-set-key [?\A-i] 'insert-e-acute)
  (global-set-key [?\A-I] 'insert-E-acute)
  (global-set-key [?\A-\\] 'insert-U-umlaut)
  (global-set-key [?\A-1] 'insert-plus-or-minus-sign)
  (global-set-key [?\A-2] 'insert-superscript-two)
  (global-set-key [?\A-3] 'insert-superscript-three)
  (global-set-key [?\A-4] 'insert-acute-accent)
  (global-set-key [?\A-5] 'insert-micro-sign)
  (global-set-key [?\A-6] 'insert-pilcrow)
  (global-set-key [?\A-7] 'insert-middle-dot)
  (global-set-key [?\A-8] 'insert-cedilla)
  (global-set-key [?\A-9] 'insert-superscript-one)
  (global-set-key [?\A-0] 'insert-ordinal-indicator-masculine)
  (global-set-key [?\A--] 'insert-soft-hyphen)
  (global-set-key [?\A-=] 'insert-fraction-one-half)
  (global-set-key [?\A-q] 'insert-n-tilde)
  (global-set-key [?\A-w] 'insert-division-sign)
  (global-set-key [?\A-e] 'insert-a-ring)
  (global-set-key [?\A-r] 'insert-o-grave)
  (global-set-key [?\A-t] 'insert-o-circumflex)
  (global-set-key [?\A-y] 'insert-u-grave)
  (global-set-key [?\A-u] 'insert-o-tilde)
  (global-set-key [?\A-i] 'insert-e-acute)
  (global-set-key [?\A-o] 'insert-i-umlaut)
  (global-set-key [?\A-p] 'insert-d-stroke)
  (global-set-key [?\A-[] 'insert-U-circumflex)
  (global-set-key [?\A-]] 'insert-Y-acute)
  (global-set-key [?\A-a] 'insert-a-acute)
  (global-set-key [?\A-s] 'insert-o-acute)
  (global-set-key [?\A-d] 'insert-a-umlaut)
  (global-set-key [?\A-f] 'insert-ae)
  (global-set-key [?\A-g] 'insert-c-cedilla)
  (global-set-key [?\A-h] 'insert-e-grave)
  (global-set-key [?\A-j] 'insert-e-circumflex)
  (global-set-key [?\A-k] 'insert-e-umlaut)
  (global-set-key [?\A-l] 'insert-i-grave)
  (global-set-key [?\A-;] 'insert-angle-quotation-mark-right)
  (global-set-key [?\A-'] 'insert-section-sign)
  (global-set-key [?\A-`] 'insert-a-grave)
  (global-set-key [?\A-z] 'insert-u-acute)
  (global-set-key [?\A-x] 'insert-o-slash)
  (global-set-key [?\A-c] 'insert-a-tilde)
  (global-set-key [?\A-v] 'insert-o-umlaut)
  (global-set-key [?\A-b] 'insert-a-circumflex)
  (global-set-key [?\A-n] 'insert-i-circumflex)
  (global-set-key [?\A-m] 'insert-i-acute)
  (global-set-key [?\A-,] 'insert-not-sign)
  (global-set-key [?\A-.] 'insert-registered-sign)
  (global-set-key [?\A-/] 'insert-macron)
)

(provide 'alt-key)