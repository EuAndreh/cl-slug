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
  (is (slugify "This, That & the Other! Various Outré Considerations")
      "this-that-the-other-various-outre-considerations"
      "Wikipedia (http://en.wikipedia.org/wiki/Semantic_URL#Slug) example works.")
  (is (slugify "String with chars from many languages: ø, å, ä, ß, ñ, ĉ, ŝ, ê, ç, ó, õ, æ, ï, ü and ö")
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
      "Eu Andre!"
      "asciify works, removing portuguese accentuation."))

(deftest CamelCaseFyTest
  (is (CamelCaseFy "Eu Andrë! with german special char: ß")
      "EuAndreWithGermanSpecialCharss"
      "CamelCaseFy works, making a CamelCaseString with ASCII characters only."))

(deftest smallCamelCaseFyTest
  (is (CamelCaseFy "Eu Andrë! with german special char: ß")
      "euAndreWithGermanSpecialCharss"
      "CamelCaseFy works, making a CamelCaseString with ASCII characters only."))

(deftest snakefy_test
  (is (snakefy "Eu Andrë! with german special char: ß")
      "eu_andre_with_german_special_char_ss"
      "snakefy works with optional CHARSET, making a snake_string with ASCII characters only."))

(run-test-all)
