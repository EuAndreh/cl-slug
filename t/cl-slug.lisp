(in-package cl-user)
(defpackage cl-slug-test
  (:use cl cl-slug prove))
(in-package cl-slug-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-slug)' in your Lisp.

(plan 3)

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

(deftest slugify-test
  (is (slugify "My new cool article, for the blog (V. 2).")
      "my-new-cool-article-for-the-blog-v-2"
      "Works with generic article title.")
  (is (slugify "This, That & the Other! Various Outré Considerations" :fr)
      "this-that-the-other-various-outre-considerations"
      "Wikipedia (http://en.wikipedia.org/wiki/Semantic_URL#Slug) example works.")
  (is (slugify "String with chars from many languages: ø, å, ä, ß, ñ, ĉ, ŝ, ê, ç, ó, õ, æ, ü and ö" :all)
      "string-with-chars-from-many-languages-ø-aa-a-ss-ñ-c-s-ê-ç-ó-õ-ae-u-and-o"
      "Works with the :ALL option.")
  (let ((slugged-string "my-string"))
    (is (slugify slugged-string)
        slugged-string
        "Doesn't mess with an already #'SLUGIFied string.")))

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

(run-test-all)
