(defsystem cl-slug-test
  :name "cl-slug-test"
  :version "0.4.1"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-slug"
  :bug-tracker "https://github.com/EuAndreh/cl-slug/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-slug.git")
  :license "LLGPL"
  :description "Test system for cl-slug."
  :depends-on (cl-slug
               prove)
  :components ((:module "t"
                        :components ((:test-file "cl-slug"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
