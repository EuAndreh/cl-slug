(in-package cl-user)
(defpackage cl-slug-test
  (:use cl cl-slug prove))
(in-package cl-slug-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-slug)' in your Lisp.

(plan 6)

(deftest test-change-*slug-separator*
  (let ((*slug-separator* #\_))
    (is (slugify "Testing the *slug-separator* var...")
        "testing_the_slug_separator_var"
        "Binding *SLUG-SEPARATOR* with LET works."))

  (is (slugify "Before the SETF.")
      "before-the-setf"
      "Out of the LET it works back normally.")

  (setf *slug-separator* #\.)
  (is (slugify "Using (setf *slug-separator* #\\.)...")
      "using.setf.slug.separator"
      "SETFing *SLUG-SEPARATOR* works.")
  (setf *slug-separator* #\-)
  (is (slugify "Using (setf *slug-separator* #\\-) to change back...")
      "using-setf-slug-separator-to-change-back"
      "Changing back with SETF also works."))

(deftest slugify-test-languages
  (is (slugify "My new cool article, for the blog (V. 2).")
      "my-new-cool-article-for-the-blog-v-2"
      "Works with generic article title.")
  (is (slugify "This, That & the Other! Various Outré Considerations" :fr)
      "this-that-the-other-various-outre-considerations"
      "Wikipedia (http://en.wikipedia.org/wiki/Semantic_URL#Slug) example works.")
  (is (slugify "String with chars from many languages: ø, å, ä, ß, ñ, ĉ, ŝ, ê, ç, ó, õ, æ, ï, ü and ö" :all)
      "string-with-chars-from-many-languages-o-aa-a-ss-n-c-s-e-c-o-o-ae-i-u-and-o"
      "Works with the :ALL option.")
  (let ((slugged-string "my-string"))
    (is (slugify slugged-string)
        slugged-string
        "Doesn't mess with an already #'SLUGIFied string.")))

(deftest string-with-numbers-test
  (is (slugify "one2three4five")
      "one2three4five"
      "#'SLUGIFY doesn't mess with numbers in the string."))

(deftest asciify-test
  (is (asciify "Eu André!")
      "Eu André!"
      "The default (:en) charset works, without removing any accentuation.")
  (is (asciify "Eu André!" :pt)
      "Eu Andre!"
      "The optional (:pt) charset works, removing portuguese accentuation."))


(import '(cl-slug::add-language
            cl-slug::*available-languages*
            cl-slug::*special-chars-alist*
            cl-slug::*accentuation-alist*
            cl-slug::*slug-separator*
            cl-slug::%langname->accentuation-alist
            cl-slug::%langname->special-chars-alist))

(deftest add-language-test
  (let (*available-languages*
        *special-chars-alist*
        *accentuation-alist*
        (%langname->accentuation-alist (make-hash-table))
        (%langname->special-chars-alist (make-hash-table)))
    (is-expand
     (add-language "English" :en ())
     (PROGN
       (PUSHNEW (CONS :EN "English")
                *AVAILABLE-LANGUAGES* :KEY #'CAR)
       (SETF (GETHASH :EN %LANGNAME->ACCENTUATION-ALIST)
             'NIL
             (GETHASH :EN %LANGNAME->SPECIAL-CHARS-ALIST)
             'NIL)
       :EN)
     "Expansion of ADD-LANGUAGE with english data (language with no accentuated or special chars) is correct.")

    (is-expand
     (add-language "Français (French)" :fr
                   ((#\a . #\â) (#\e . #\ê) (#\i . #\î) (#\o . #\ô) (#\u . #\û)
                    (#\a . #\à) (#\e . #\è)                         (#\u . #\ù)
                    (#\e . #\é)
                    (#\e . #\ë) (#\i . #\ï)             (#\u . #\ü)
                    (#\c . #\ç) (#\y . #\ÿ) (#\i . #\î))
                   (("oe" . "œ") ("ae" . "æ")))
     (PROGN
       (PUSHNEW (CONS :FR "Français (French)")
                *AVAILABLE-LANGUAGES* :KEY #'CAR)
       (SETF (GETHASH :FR %LANGNAME->ACCENTUATION-ALIST)
             '((#\a . #\LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX)
               (#\e . #\LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX)
               (#\i . #\LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX)
               (#\o . #\LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX)
               (#\u . #\LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX)
               (#\a . #\LATIN_SMALL_LETTER_A_WITH_GRAVE)
               (#\e . #\LATIN_SMALL_LETTER_E_WITH_GRAVE)
               (#\u . #\LATIN_SMALL_LETTER_U_WITH_GRAVE)
               (#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
               (#\e . #\LATIN_SMALL_LETTER_E_WITH_DIAERESIS)
               (#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
               (#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS)
               (#\c . #\LATIN_SMALL_LETTER_C_WITH_CEDILLA)
               (#\y . #\LATIN_SMALL_LETTER_Y_WITH_DIAERESIS)
               (#\i . #\LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX))
             (GETHASH :FR %LANGNAME->SPECIAL-CHARS-ALIST)
             '(("oe" . "œ") ("ae" . "æ")))
       (PUSHNEW '(#\a . #\LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\e . #\LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\i . #\LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\o . #\LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\u . #\LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\a . #\LATIN_SMALL_LETTER_A_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\e . #\LATIN_SMALL_LETTER_E_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\u . #\LATIN_SMALL_LETTER_U_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\e . #\LATIN_SMALL_LETTER_E_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\c . #\LATIN_SMALL_LETTER_C_WITH_CEDILLA)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\y . #\LATIN_SMALL_LETTER_Y_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\i . #\LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\A . #\LATIN_CAPITAL_LETTER_A_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\E . #\LATIN_CAPITAL_LETTER_E_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\I . #\LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\O . #\LATIN_CAPITAL_LETTER_O_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\U . #\LATIN_CAPITAL_LETTER_U_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\A . #\LATIN_CAPITAL_LETTER_A_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\E . #\LATIN_CAPITAL_LETTER_E_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\U . #\LATIN_CAPITAL_LETTER_U_WITH_GRAVE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\E . #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\E . #\LATIN_CAPITAL_LETTER_E_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\I . #\LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\U . #\LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\C . #\LATIN_CAPITAL_LETTER_C_WITH_CEDILLA)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\Y . #\LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\I . #\LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '("oe" . "œ")
                (GETHASH :ALL %LANGNAME->SPECIAL-CHARS-ALIST) :KEY #'CDR)
       (PUSHNEW '("ae" . "æ")
                (GETHASH :ALL %LANGNAME->SPECIAL-CHARS-ALIST) :KEY #'CDR)
       (PUSHNEW '("OE" . "Œ")
                (GETHASH :ALL %LANGNAME->SPECIAL-CHARS-ALIST) :KEY #'CDR)
       (PUSHNEW '("AE" . "Æ")
                (GETHASH :ALL %LANGNAME->SPECIAL-CHARS-ALIST) :KEY #'CDR)
       :FR)
     "Expansion of ADD-LANGUAGE with french data (language with accentuated chars and special chars) is correct.")

    (is-expand
     (add-language "Español (Spanish)" :es
                   ((#\a . #\á) (#\e . #\é) (#\i . #\í) (#\o . #\ó) (#\u . #\ú)
                    (#\n . #\ñ)             (#\i . #\ï)             (#\u . #\ü)))
     (PROGN
       (PUSHNEW (CONS :ES "Español (Spanish)")
                *AVAILABLE-LANGUAGES* :KEY #'CAR)
       (SETF (GETHASH :ES %LANGNAME->ACCENTUATION-ALIST)
             '((#\a . #\LATIN_SMALL_LETTER_A_WITH_ACUTE)
               (#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
               (#\i . #\LATIN_SMALL_LETTER_I_WITH_ACUTE)
               (#\o . #\LATIN_SMALL_LETTER_O_WITH_ACUTE)
               (#\u . #\LATIN_SMALL_LETTER_U_WITH_ACUTE)
               (#\n . #\LATIN_SMALL_LETTER_N_WITH_TILDE)
               (#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
               (#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS))
             (GETHASH :ES %LANGNAME->SPECIAL-CHARS-ALIST)
             'NIL)
       (PUSHNEW '(#\a . #\LATIN_SMALL_LETTER_A_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\i . #\LATIN_SMALL_LETTER_I_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\o . #\LATIN_SMALL_LETTER_O_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\u . #\LATIN_SMALL_LETTER_U_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\n . #\LATIN_SMALL_LETTER_N_WITH_TILDE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\A . #\LATIN_CAPITAL_LETTER_A_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\E . #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\I . #\LATIN_CAPITAL_LETTER_I_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\O . #\LATIN_CAPITAL_LETTER_O_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\U . #\LATIN_CAPITAL_LETTER_U_WITH_ACUTE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\N . #\LATIN_CAPITAL_LETTER_N_WITH_TILDE)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\I . #\LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       (PUSHNEW '(#\U . #\LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS)
                (GETHASH :ALL %LANGNAME->ACCENTUATION-ALIST) :KEY #'CDR)
       :ES)
     "Expansion of ADD-LANGUAGE with spanish data (language with accentuated chars but no special chars) is correct.")

    (add-language "Español (Spanish)" :es
                  ((#\a . #\á) (#\e . #\é) (#\i . #\í) (#\o . #\ó) (#\u . #\ú)
                   (#\n . #\ñ)             (#\i . #\ï)             (#\u . #\ü)))
    (add-language "English" :en () )
    (add-language "Français (French)" :fr
                  ((#\a . #\â) (#\e . #\ê) (#\i . #\î) (#\o . #\ô) (#\u . #\û)
                   (#\a . #\à) (#\e . #\è)                         (#\u . #\ù)
                   (#\e . #\é)
                   (#\e . #\ë) (#\i . #\ï)             (#\u . #\ü)
                   (#\c . #\ç) (#\y . #\ÿ) (#\i . #\î))
                  (("oe" . "œ") ("ae" . "æ")))
    (is *available-languages*
        '((:FR . "Français (French)") (:EN . "English") (:ES . "Español (Spanish)"))
        "*AVAILABLE-LANGUAGES* special var has the expected values: :fr, :en and :es.")
    (is *special-chars-alist*
        NIL
        "*SPECIAL-CHARS-ALIST* special var has the expected global value: NIL.")
    (is *accentuation-alist*
        NIL
        "*ACCENTUATION-ALIST* special var has the expected global value: NIL.")
    (let ((*special-chars-alist* (gethash :es %langname->special-chars-alist))
          (*accentuation-alist* (gethash :es %langname->accentuation-alist)))
      (is *special-chars-alist*
          NIL
          "*SPECIAL-CHARS-ALIST* special var has the expected :ES value: NIL.")
      (is *accentuation-alist*
          '((#\a . #\LATIN_SMALL_LETTER_A_WITH_ACUTE)
            (#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
            (#\i . #\LATIN_SMALL_LETTER_I_WITH_ACUTE)
            (#\o . #\LATIN_SMALL_LETTER_O_WITH_ACUTE)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_ACUTE)
            (#\n . #\LATIN_SMALL_LETTER_N_WITH_TILDE)
            (#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS))
          "*ACCENTUATION-ALIST* special var has the expected :ES value: the Spanish accentuation alist."))
    (let ((*special-chars-alist* (gethash :all %langname->special-chars-alist))
          (*accentuation-alist* (gethash :all %langname->accentuation-alist)))
      (is *special-chars-alist*
          '(("AE" . "Æ") ("OE" . "Œ") ("ae" . "æ") ("oe" . "œ"))
          "*SPECIAL-CHARS-ALIST* special var has the expected :ALL value: the union of all given special chars (only french, in this case).")
      (is *accentuation-alist*
          '((#\Y . #\LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS)
            (#\C . #\LATIN_CAPITAL_LETTER_C_WITH_CEDILLA)
            (#\E . #\LATIN_CAPITAL_LETTER_E_WITH_DIAERESIS)
            (#\U . #\LATIN_CAPITAL_LETTER_U_WITH_GRAVE)
            (#\E . #\LATIN_CAPITAL_LETTER_E_WITH_GRAVE)
            (#\A . #\LATIN_CAPITAL_LETTER_A_WITH_GRAVE)
            (#\U . #\LATIN_CAPITAL_LETTER_U_WITH_CIRCUMFLEX)
            (#\O . #\LATIN_CAPITAL_LETTER_O_WITH_CIRCUMFLEX)
            (#\I . #\LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX)
            (#\E . #\LATIN_CAPITAL_LETTER_E_WITH_CIRCUMFLEX)
            (#\A . #\LATIN_CAPITAL_LETTER_A_WITH_CIRCUMFLEX)
            (#\y . #\LATIN_SMALL_LETTER_Y_WITH_DIAERESIS)
            (#\c . #\LATIN_SMALL_LETTER_C_WITH_CEDILLA)
            (#\e . #\LATIN_SMALL_LETTER_E_WITH_DIAERESIS)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_GRAVE)
            (#\e . #\LATIN_SMALL_LETTER_E_WITH_GRAVE)
            (#\a . #\LATIN_SMALL_LETTER_A_WITH_GRAVE)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX)
            (#\o . #\LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX)
            (#\i . #\LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX)
            (#\e . #\LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX)
            (#\a . #\LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX)
            (#\U . #\LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS)
            (#\I . #\LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS)
            (#\N . #\LATIN_CAPITAL_LETTER_N_WITH_TILDE)
            (#\U . #\LATIN_CAPITAL_LETTER_U_WITH_ACUTE)
            (#\O . #\LATIN_CAPITAL_LETTER_O_WITH_ACUTE)
            (#\I . #\LATIN_CAPITAL_LETTER_I_WITH_ACUTE)
            (#\E . #\LATIN_CAPITAL_LETTER_E_WITH_ACUTE)
            (#\A . #\LATIN_CAPITAL_LETTER_A_WITH_ACUTE)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS)
            (#\i . #\LATIN_SMALL_LETTER_I_WITH_DIAERESIS)
            (#\n . #\LATIN_SMALL_LETTER_N_WITH_TILDE)
            (#\u . #\LATIN_SMALL_LETTER_U_WITH_ACUTE)
            (#\o . #\LATIN_SMALL_LETTER_O_WITH_ACUTE)
            (#\i . #\LATIN_SMALL_LETTER_I_WITH_ACUTE)
            (#\e . #\LATIN_SMALL_LETTER_E_WITH_ACUTE)
            (#\a . #\LATIN_SMALL_LETTER_A_WITH_ACUTE))
          "*ACCENTUATION-ALIST* special var has the expected :ALL value: the union of all given accentuated chars (french + spanish, in this case, without repeated entries)."))))

(deftest invalid-charset-error-test
  (is-error (asciify "ASCII string" :jp)
            (or invalid-charset-error simple-error error)
            "INVALID-CHARSET-ERROR is thrown with SLUGIFY.")
  (is-error (asciify "ASCII string" :jp)
            (or invalid-charset-error simple-error error)
            "INVALID-CHARSET-ERROR is thrown with ASCIIFY."))

(run-test-all)
