(in-package cl-user)
(defpackage cl-slug-test
  (:use cl cl-slug prove))
(in-package cl-slug-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-slug)' in your Lisp.

(plan 10)

(deftest test-*accentuation-alist*-pairs-equivalence
  (let ((accentuated-side (with-output-to-string (s)
                            (mapcar (lambda (pair)
                                      (princ (cdr pair) s))
                                    *accentuation-alist*)))
        (unaccentuated-side (with-output-to-string (s)
                              (mapcar (lambda (pair)
                                        (princ (car pair) s))
                                      *accentuation-alist*))))

    (is (remove-accentuation accentuated-side)
        unaccentuated-side
        "*ACCENTUATION-ALIST* pairs match.")))

(deftest add-to-*accentuation-alist*
  (let ((test-string "ć"))
   (is (remove-accentuation test-string)
       test-string
       "Doesn't remove accentuation for a character outside *ACCENTUATION-ALIST*.")
   (let ((*accentuation-alist* (cons '(#\c . #\ć) *accentuation-alist*)))
     (is (remove-accentuation test-string)
         "c"
         "(pushnew '(chars) *accentuation-alist*) works for #'REMOVE-ACCENTUATION."))))

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

(deftest remove-accentuation-test
  (is (remove-accentuation "André Miranda!")
      "Andre Miranda!"
      "Works, without changing anything else.")
  (let ((string-with-no-accentuation "String with no accentuation."))
   (is (remove-accentuation string-with-no-accentuation)
       string-with-no-accentuation
       "Doesn't change the string when it has no accentuation.")))

(deftest remove-ponctuation-test
  (is (remove-ponctuation "André Miranda!")
      "André-Miranda"
      "Changes the ponctuation without changing the accentuation or anything else.")
  (let ((string-with-no-ponctuation "StringWithNoPonctuation"))
    (is (remove-ponctuation string-with-no-ponctuation)
        string-with-no-ponctuation
        "Doesn't change the string when it has no ponctuation")))

(deftest slugify-test
  (is (slugify "My new cool article, for the blog (V. 2).")
      "my-new-cool-article-for-the-blog-v-2"
      "Works with generic article title.")
  (is (slugify "This, That & the Other! Various Outré Considerations")
      "this-that-the-other-various-outre-considerations"
      "Wikipedia (http://en.wikipedia.org/wiki/Semantic_URL#Slug) example works.")
  (let ((slugged-string "my-string"))
    (is (slugify slugged-string)
        slugged-string
        "Doesn't mess with an already #'SLUGIFied string.")))

(deftest slugify-en-test
  (is (slugify-en "My new cool article, for the blog (V. 2).")
      "my-new-cool-article-for-the-blog-v-2"
      "Works with generic article title.")
  (is (slugify-en "This, That & the Other! Various Outré Considerations")
      "this-that-the-other-various-outré-considerations"
      "Works, but doesn't remove accentuated chars, as expected.")
  (let ((slugged-string "my-string"))
    (is (slugify-en slugged-string)
        slugged-string
        "Doesn't mess with an already #'SLUGIFied string.")))

(deftest remove-special-chars-test
  (is (remove-special-chars "Groß")
      "Gross"
      "#'REMOVE-SPECIAL-CHAR works correctly."))

(deftest string-with-numbers-test
  (let ((numbered-string "one2three4five"))
    (is (remove-accentuation numbered-string)
        numbered-string
        "#'REMOVE-ACCENTUATION doesn't mess with numbers in the string.")
    (is (remove-ponctuation numbered-string)
        numbered-string
        "#'REMOVE-PONCTUATION doesn't mess with numbers in the string.")
    (is (slugify numbered-string)
        numbered-string
        "#'SLUGIFY doesn't mess with numbers in the string.")))

(deftest test-remove-*-independence
  (let ((string-example "A string with accentuation (á, é and Ü), ponctuation (!, ? and the parentheses =p ), and special chars (ß, œ and æ)."))
    (is (remove-accentuation  (remove-ponctuation   string-example))
        (remove-ponctuation   (remove-accentuation  string-example))
        "#'REMOVE-ACCENTUATION and #'REMOVE-PONCTUATION doesn't mess with each other.")

    (is (remove-accentuation  (remove-special-chars string-example))
        (remove-special-chars (remove-accentuation  string-example))
        "#'REMOVE-ACCENTUATION and #'REMOVE-SPECIAL-CHARS doesn't mess with each other.")
    (is (remove-ponctuation   (remove-special-chars string-example))
        (remove-special-chars (remove-ponctuation   string-example))
        "#'REMOVE-PONCTUATION and #'REMOVE-SPECIAL-CHARS doesn't mess with each other.")))

(deftest test-multi-language-slugify
  (is (slugify "A string with special chars (like ! and ?), portuguese (á), french (è), esperanto (ŭ), german (ß) and swedish (å) related stuff.")
      "a-string-with-special-chars-like-and-portuguese-a-french-e-esperanto-u-german-ss-and-swedish-a-related-stuff"
      "Slugify seems to handle all (claimed to be) supported languages."))

(run-test-all)
