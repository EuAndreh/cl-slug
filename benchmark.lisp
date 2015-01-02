#!/usr/local/bin/sbcl --script
(load "~/.sbclrc") ;; load the quicklisp initalization form
(ql:quickload :cl-slug)

(in-package cl-user)
(defpackage cl-slug-benckmark
  (:use cl cl-slug))
(in-package cl-slug-benckmark)

(defparameter string-example-special "A string with accentuation (á, é and Ü), ponctuation (!, #, ? and the parentheses =p ), and special chars (ß, œ and æ).")

(defparameter string-example-ascii "A string with accentuation, ponctuation or special characters whatsoever.")

(defun cl-slug-benchmark (n)
  (format *trace-output*
          "~&#'SLUGIFY benchmark with special char and accentuation:~%")
  (time (dotimes (i n)
          (slugify string-example-special)))

  (format *trace-output*
          "#'SLUGIFY-EN benchmark with special char and accentuation:~%")
  (time (dotimes (i n)
          (slugify-en string-example-special)))

  (format *trace-output*
          "#'SLUGIFY benchmark without special char or accentuation:~%")
  (time (dotimes (i n)
          (slugify string-example-ascii)))

  (format *trace-output*
          "#'SLUGIFY-EN benchmark without special char or accentuation:~%")
  (time (dotimes (i n)
          (slugify-en string-example-ascii))))

(princ "Compiling benchmark...")
(compile 'cl-slug-benchmark)
(princ "done.")

(princ "Running benchmarks...")
(cl-slug-benchmark 100)
(princ "done.")
(princ #\Newline)
