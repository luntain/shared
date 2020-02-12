
;; load the official prettify symbols for pragmata pro (with some ligations commented out)
(load "~/shared/pragmatapro-prettify-symbols-for-pp-v0.828.el")

(add-hook 'prog-mode-hook
          #'prettify-hook)

(global-prettify-symbols-mode +1)

(defconst pragmatapro-fontlock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ,(concat (list ?\C-i)
                                            (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(
            ; alternative arrows available in pragmata pro, apart from the default: #XE984
            ;("\\(0>\\)"        #XE984)
            ;("\\(a>\\)"        #X2192)
            ;(" \\(b>\\) "        #X1F802)
            ;(" \\(c>\\) "        #X27B5)
            ;(" \\(d>\\) "        #X2B62)
            ;(" \\(e>\\) "        #XE8E2)
            ;(" \\(f>\\) "        #X1F862)

            ; those alternative arrows couldn't be substituted using prettify symbols
            (" \\(<-\\) "        #X1F850)
            (" \\(->\\) "        #X1F852)

            (" \\(::\\) "        #X2208)
            ;("\\(forall\\) "     #X2200)
            ("\\(forall\\) "     #X22C0)
            ; a nicer, smaller <>
            ("\\(<>\\) "         #X22C4)
            ("\\(\\\\\\)[^-]+ -"  #X03BB)
            )))

(defun add-pragmatapro-symbol-keywords ()
  (font-lock-add-keywords nil pragmatapro-fontlock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-pragmatapro-symbol-keywords)

(set-frame-font "PragmataPro Liga 14" nil t)
