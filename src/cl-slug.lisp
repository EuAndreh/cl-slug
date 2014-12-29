(in-package cl-user)
(defpackage cl-slug
  (:use cl)
  (:export *accentuation-alist*
           *slug-separator*
           remove-accentuation
           remove-ponctuation
           remove-special-chars
           slugify
           slugify-en)
  (:documentation "Main (and only) package."))
(in-package cl-slug)

(defparameter *special-chars-alist* '(("ss" . "ß") ("oe" . "œ") ("ae" . "æ"))
  "Alist with special chars that beahve differently from others.")

(defparameter *accentuation-alist*
  (let ((chars '((#\A . #\Á) (#\E . #\É) (#\I . #\Í) (#\O . #\Ó) (#\U . #\Ú)
                 (#\A . #\Â) (#\E . #\È) (#\I . #\Î) (#\O . #\Ô) (#\U . #\Ü)
                 (#\A . #\À) (#\E . #\Ê) (#\I . #\Ï) (#\O . #\Õ) (#\U . #\Ŭ)
                 (#\A . #\Ã) (#\E . #\Ë)             (#\O . #\Ö) (#\U . #\Ù)
                 (#\A . #\Ä)                         (#\O . #\Ô) (#\U . #\Û)
                                                     (#\O . #\Ö)

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

(defun remove-special-chars (string)
  "Removes all special characters stored in *SPECIAL-CHARS-ALIST* using #'PPCRE:REGEX-REPLACE-ALL."
  (labels ((rec (chars-list str)
             (if chars-list
                 (let ((char-pair (car chars-list)))
                   (rec (cdr chars-list)
                        (ppcre:regex-replace-all (cdr char-pair)
                                                 str
                                                 (car char-pair))))
                 str)))
    (rec *special-chars-alist* string)))

(defun slugify (string)
  "Makes STRING a slug: a downcase string, with no special characters, ponctuation or accentuated letters whatsoever."
  (remove-accentuation
   (string-downcase
    (remove-special-chars
     (remove-ponctuation string)))))

(defun slugify-en (string)
  "Does the same job as #'SLUGIFY, but works only with the ASCII charset. If it finds any char from outside of ASCII it doesn't remove it. Use it to slugify english strings faster."
  (string-downcase (remove-ponctuation string)))
