(in-package :cl-user)
(defpackage cl-slug-asd
  (:use cl asdf))
(in-package cl-slug-asd)

(defsystem cl-slug
  :version "0.3"
  :author "André Miranda <andremiramor@gmail.com>"
  :license "LLGPL"
  :depends-on (cl-ppcre
               split-sequence)
  :components ((:module "src"
                :components
                ((:file "cl-slug"))))
  :description "Small library to make slugs, mainly for URIs, from english and beyond."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op cl-slug-test))))
