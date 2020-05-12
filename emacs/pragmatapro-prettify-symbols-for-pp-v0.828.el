;; This is taken from
;; https://github.com/fabrizioschiavi/pragmatapro/blob/master/emacs_snippets/pragmatapro-prettify-symbols-v0.828.el
;; I have applied some modifications:
;;   * commented out some ligations that I have overriden using font lock in a different 'el' file
;;   * commented out some more obscure ones, not to slow Emacs
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(
            ;; ("[ERROR]"    #XE2C0)
            ;; ("[DEBUG]"    #XE2C1)
            ;; ("[INFO]"     #XE2C2)
            ;; ("[WARN]"     #XE2C3)
            ;; ("[WARNING]"  #XE2C4)
            ;; ("[ERR]"      #XE2C5)
            ;; ("[FATAL]"    #XE2C6)
            ;; ("[TRACE]"    #XE2C7)
            ;; ("[FIXME]"    #XE2C8)
            ;; ("[TODO]"     #XE2C9)
            ;; ("[BUG]"      #XE2CA)
            ;; ("[NOTE]"     #XE2CB)
            ;; ("[HACK]"     #XE2CC)
            ;; ("[MARK]"     #XE2CD)
            ;; ("# ERROR"    #XE2F0)
            ;; ("# DEBUG"    #XE2F1)
            ;; ("# INFO"     #XE2F2)
            ;; ("# WARN"     #XE2F3)
            ;; ("# WARNING"  #XE2F4)
            ;; ("# ERR"      #XE2F5)
            ;; ("# FATAL"    #XE2F6)
            ;; ("# TRACE"    #XE2F7)
            ;; ("# FIXME"    #XE2F8)
            ;; ("# TODO"     #XE2F9)
            ;; ("# BUG"      #XE2FA)
            ;; ("# NOTE"     #XE2FB)
            ;; ("# HACK"     #XE2FC)
            ;; ("# MARK"     #XE2FD)
            ;; ("// ERROR"   #XE2E0)
            ;; ("// DEBUG"   #XE2E1)
            ;; ("// INFO"    #XE2E2)
            ;; ("// WARN"    #XE2E3)
            ;; ("// WARNING" #XE2E4)
            ;; ("// ERR"     #XE2E5)
            ;; ("// FATAL"   #XE2E6)
            ;; ("// TRACE"   #XE2E7)
            ;; ("// FIXME"   #XE2E8)
            ;; ("// TODO"    #XE2E9)
            ;; ("// BUG"     #XE2EA)
            ;; ("// NOTE"    #XE2EB)
            ;; ("// HACK"    #XE2EC)
            ;; ("// MARK"    #XE2ED)
            ;; ("!!"         #XE900)
            ("!="         #XE901)
            ;; ("!=="        #XE902)
            ;; ("!!!"        #XE903)
            ;; ("!≡"         #XE904)
            ;; ("!≡≡"        #XE905)
            ;; ("!>"         #XE906)
            ;; ("!=<"        #XE907)
            ;; ("#("         #XE920)
            ;; ("#_"         #XE921)
            ;; ("#{"         #XE922)
            ;; ("#?"         #XE923)
            ;; ("#>"         #XE924)
            ;; ("##"         #XE925)
            ;; ("#_("        #XE926)
            ("%="         #XE930)
            ;; ("%>"         #XE931)
            ;; ("%>%"        #XE932)
            ;; ("%<%"        #XE933)
            ;; ("&%"         #XE940)
            ("&&"         #XE941)
            ;; ("&*"         #XE942)
            ;; ("&+"         #XE943)
            ;; ("&-"         #XE944)
            ;; ("&/"         #XE945)
            ;; ("&="         #XE946)
            ("&&&"        #XE947)
            ;; ("&>"         #XE948)
            ;; ("$>"         #XE955)
            ("***"        #XE960)
            ("*="         #XE961)
            ;; ("*/"         #XE962)
            ("*>"         #XE963)
            ("++"         #XE970)
            ;; ("+++"        #XE971)
            ("+="         #XE972)
            ;; ("+>"         #XE973)
            ;; ("++="        #XE974)
            ("--"         #XE980)
            ("-<"         #XE981)
            ("-<<"        #XE982)
            ("-="         #XE983)
            ;("->"         #XE984)
            ("->>"        #XE985)
            ("---"        #XE986)
            ("-->"        #XE987)
            ;; ("-+-"        #XE988)
            ;; ("-\\/"       #XE989)
            ;; ("-|>"        #XE98A)
            ;; ("-<|"        #XE98B)
            (".."         #XE990)
            ("..."        #XE991)
            ;; ("..<"        #XE992)
            ;; (".>"         #XE993)
            (".~"         #XE994)
            (".="         #XE995)
            ("/*"         #XE9A0)
            ;; ("//"         #XE9A1)
            ;; ("/>"         #XE9A2)
            ("/="         #XE9A3)
            ("/=="        #XE9A4)
            ;; ("///"        #XE9A5)
            ;; ("/**"        #XE9A6)
            ;; (":::"        #XE9AF)
            ;("::"         #XE9B0)
            (":="         #XE9B1)
            ;; (":≡"         #XE9B2)
            (":>"         #XE9B3)
            ;; (":=>"        #XE9B4)
            ;; (":("         #XE9B5)
            ;; (":-("        #XE9B6)
            ;; (":)"         #XE9B7)
            ;; (":-)"        #XE9B8)
            ;; (":/"         #XE9B9)
            ;; (":\\"        #XE9BA)
            ;; (":3"         #XE9BB)
            ;; (":D"         #XE9BC)
            ;; (":P"         #XE9BD)
            ;; (":>:"        #XE9BE)
            ;; (":<:"        #XE9BF)
            ("<$>"        #XE9C0)
            ("<*"         #XE9C1)
            ("<*>"        #XE9C2)
            ("<+>"        #XE9C3)
            ;("<-"         #XE9C4)
            ("<<"         #XE9C5)
            ("<<<"        #XE9C6)
            ("<<="        #XE9C7)
            ("<="         #XE9C8)
            ("<=>"        #XE9C9)
            ;("<>"         #XE9CA)
            ("<|>"        #XE9CB)
            ;; ("<<-"        #XE9CC)
            ("<|"         #XE9CD)
            ("<=<"        #XE9CE)
            ("<~"         #XE9CF)
            ("<~~"        #XE9D0)
            ("<<~"        #XE9D1)
            ("<$"         #XE9D2)
            ("<+"         #XE9D3)
            ("<!>"        #XE9D4)
            ("<@>"        #XE9D5)
            ;; ("<#>"        #XE9D6)
            ;; ("<%>"        #XE9D7)
            ;; ("<^>"        #XE9D8)
            ;; ("<&>"        #XE9D9)
            ;; ("<?>"        #XE9DA)
            ("<.>"        #XE9DB)
            ("</>"        #XE9DC)
            ;; ("<\\>"       #XE9DD)
            ;; ("<\">"       #XE9DE)
            ("<:>"        #XE9DF)
            ;; ("<~>"        #XE9E0)
            ("<**>"       #XE9E1)
            ("<<^"        #XE9E2)
            ("<!"         #XE9E3)
            ("<@"         #XE9E4)
            ("<#"         #XE9E5)
            ;; ("<%"         #XE9E6)
            ;; ("<^"         #XE9E7)
            ;; ("<&"         #XE9E8)
            ;; ("<?"         #XE9E9)
            ;; ("<."         #XE9EA)
            ;; ("</"         #XE9EB)
            ;; ("<\\"        #XE9EC)
            ;; ("<\""        #XE9ED)
            ("<:"         #XE9EE)
            ("<->"        #XE9EF)
            ("<!--"       #XE9F0)
            ("<--"        #XE9F1)
            ("<~<"        #XE9F2)
            ("<==>"       #XE9F3)
            ;; ("<|-"        #XE9F4)
            ("<<|"        #XE9F5)
            ("<-<"        #XE9F7)
            ;; ("<-->"       #XE9F8)
            ;; ("<<=="       #XE9F9)
            ;; ("<=="        #XE9FA)
            ("=<<"        #XEA00)
            ("=="         #XEA01)
            ("==="        #XEA02)
            ("==>"        #XEA03)
            ("=>"         #XEA04)
            ;; ("=~"         #XEA05)
            ;; ("=>>"        #XEA06)
            ;; ("=/="        #XEA07)
            ;; ("=~="        #XEA08)
            ;; ("==>>"       #XEA09)
            ;; ("≡≡"         #XEA10)
            ;; ("≡≡≡"        #XEA11)
            ;; ("≡:≡"        #XEA12)
            (">-"         #XEA20)
            (">="         #XEA21)
            (">>"         #XEA22)
            (">>-"        #XEA23)
            (">>="        #XEA24)
            (">>>"        #XEA25)
            (">=>"        #XEA26)
            (">>^"        #XEA27)
            (">>|"        #XEA28)
            ;; (">!="        #XEA29)
            (">->"        #XEA2A)
            ;; ("??"         #XEA40)
            ;; ("?~"         #XEA41)
            ;; ("?="         #XEA42)
            ;; ("?>"         #XEA43)
            ;; ("???"        #XEA44)
            ("?."         #XEA45)
            ("^="         #XEA48)
            ("^."         #XEA49)
            ("^?"         #XEA4A)
            ("^.."        #XEA4B)
            ;; ("^<<"        #XEA4C)
            ;; ("^>>"        #XEA4D)
            ("^>"         #XEA4E)
            ;; ("\\\\"       #XEA50)
            ("\\>"        #XEA51)
            ;; ("\\/-"       #XEA52)
            ;; ("@>"         #XEA57)
            ;; ("|="         #XEA60)
            ;; ("||"         #XEA61)
            ("|>"         #XEA62)
            ("|||"        #XEA63)
            ;; ("|+|"        #XEA64)
            ;; ("|->"        #XEA65)
            ;; ("|-->"       #XEA66)
            ;; ("|=>"        #XEA67)
            ;; ("|==>"       #XEA68)
            ;; ("|>-"        #XEA69)
            ;; ("|<<"        #XEA6A)
            ;; ("||>"        #XEA6B)
            ;; ("|>>"        #XEA6C)
            ;; ("|-"         #XEA6D)
            ;; ("||-"        #XEA6E)
            ("~="         #XEA70)
            ("~>"         #XEA71)
            ("~~>"        #XEA72)
            ("~>>"        #XEA73)
            ;; ("[["         #XEA80)
            ;; ("]]"         #XEA81)
            ;; ("\">"        #XEA90)
            ;; ("_|_"        #XEA97)
            )))

(defun add-pragmatapro-prettify-symbols-alist ()
  (setq prettify-symbols-alist pragmatapro-prettify-symbols-alist))

;; enable prettified symbols on comments
(defun setup-compose-predicate ()
  (setq prettify-symbols-compose-predicate
        (defun my-prettify-symbols-default-compose-p (start end _match)
          "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
          (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\)))
                 (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\))))
            (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
                     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
                     (nth 3 (syntax-ppss))))))))

;; main hook fn, just add to text-mode/prog-mode
(defun prettify-hook ()
  (add-pragmatapro-prettify-symbols-alist)
  (setup-compose-predicate))
