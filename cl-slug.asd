(in-package cl-user)
(defpackage cl-slug-asd
  (:use cl asdf))
(in-package cl-slug-asd)

(defsystem cl-slug
  :version "0.2"
  :author "André Miranda"
  :license "LLGPL"
  :depends-on (cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "cl-slug"))))
  :description "Small library to make slugs, mainly for URIs, from english and beyond."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-slug-test))))
