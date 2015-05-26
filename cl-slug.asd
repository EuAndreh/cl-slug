(defsystem cl-slug
  :version "0.4.1"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-slug"
  :bug-tracker "https://github.com/EuAndreh/cl-slug/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-slug.git")
  :license "LLGPL"
  :description "Small library to make slugs, mainly for URIs, from english and beyond."
  :depends-on (cl-ppcre)
  :components ((:module "src"
                        :components
                        ((:file "cl-slug")))
               (:static-file "README.md"))
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-slug-test))))
