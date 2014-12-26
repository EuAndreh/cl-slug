(defpackage cl-slug
  (:use cl)
  (:export *accentuation-alist*
           *slug-separator*
           remove-accentuation
           remove-ponctuation
           slugify))
(in-package cl-slug)

(defparameter *accentuation-alist*
  (let ((chars '((#\A . #\Á) (#\E . #\É) (#\I . #\Í) (#\O . #\Ó) (#\U . #\Ú)
                 (#\A . #\Â) (#\E . #\È)             (#\O . #\Ô) (#\U . #\Ü)
                 (#\A . #\À) (#\E . #\Ê)             (#\O . #\Õ) (#\U . #\Ŭ)
                 (#\A . #\Ã)

                 (#\C . #\Ç) (#\G . #\Ĝ) (#\H . #\Ĥ) (#\J . #\Ĵ) (#\S . #\Ŝ)
                 (#\C . #\Ĉ))))
    (append chars (mapcar (lambda (pair)
                            (cons (char-downcase (car pair))
                                  (char-downcase (cdr pair))))
                          chars)))
  "Alist with accentuated chars and their respective accentless char.")

(defparameter *slug-separator* #\- "Default separator of slug strings.")

(defun remove-accentuation (string)
  "Removes accentuation (according to *ACCENTUATION-ALIST*) from STRING."
  (map 'string (lambda (char)
                 (let ((pair (rassoc char *accentuation-alist*)))
                   (if pair
                       (car pair)
                       char)))
       string))

(defun remove-ponctuation (string)
  "Removes ponctuation and other special characters from STRING according #'ALPHANUMERICP."
  (labels ((substitute-ponctuation-by-separator (string)
             "If a (alphanumericp char) returns true, such char is replaced with *SLUG-SEPARATOR*."
             (map 'string (lambda (char)
                                   (if (alphanumericp char)
                                       char
                                       *slug-separator*))
                           string))
           (remove-repeated-separator (string)
             "Removes consecutives *SLUG-SEPARATOR*s from string."
             (remove-if (let (toggle)
                          (lambda (char)
                            (if toggle
                                (if (char= char *slug-separator*)
                                    t
                                    (setf toggle nil))
                                (if (char= char *slug-separator*)
                                    (progn
                                      (setf toggle t)
                                      nil)))))
                        string)))
    (remove-repeated-separator
     (string-trim (list *slug-separator*)
                  (substitute-ponctuation-by-separator string)))))

(defun slugify (string)
  "Makes STRING a slug: a downcase string, with no special characters or ponctuation or accentuated letters whatsoever."
  (remove-accentuation (string-downcase (remove-ponctuation string))))
