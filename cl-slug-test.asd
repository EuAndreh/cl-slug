(in-package :cl-user)
(asdf:defsystem cl-slug-test
  :depends-on (cl-slug
               prove)
  :components ((:module "t"
                        :components
                        ((:test-file "cl-slug"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
