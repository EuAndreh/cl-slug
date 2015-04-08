(in-package cl-user)
(defpackage cl-slug-test-asd
  (:use cl asdf))
(in-package cl-slug-test-asd)

(defsystem cl-slug-test
  :author "André Miranda <andremiramor@gmail.com>"
  :license "LLGPL"
  :mailto "<andremiramor@gmail.com>"
  :homepage "https://github.com/EuAndreh/fad-iter"
  :depends-on (cl-slug
               prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-slug"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
