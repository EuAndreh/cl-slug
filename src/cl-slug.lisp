(defpackage cl-slug
  (:use cl)
  (:nicknames slug)
  (:export slugify
           asciify
           CamelCaseFy
           snakefy
           *slug-separator*)
  (:documentation "Main (and only) package. Package nickname SLUG also available."))
(in-package cl-slug)



(defparameter *available-languages* ()
  "Alist with (KEY . LANGUAGE-NAMESTRING).")

(defparameter *slug-separator* #\- "Default separator of slug strings.")

(defparameter %accentuations (make-hash-table)
  "Hash table of all {accentuation -> ascii-equivalent} characters.")

(defparameter %special-chars (make-hash-table)
  "Hash table of all {special-char -> ascii-equivalent} strings.")

(defmacro add-language (name key-code accentuation-alist
                        &optional special-chars-alist)
  (flet ((add-downcase-equivalent (alist)
           "Adds an equivalent downcase cons pair to every cons pair."
           (remove-duplicates
            (append alist (mapcar (lambda (pair)
                                    (cons (string-downcase (car pair))
                                          (string-downcase (cdr pair))))
                                  alist)))))
    `(progn
       (pushnew (cons ,key-code ,name) *available-languages* :key #'car)
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(string (cdr pair)) %accentuations)
                          ,(string (car pair))))
                 (add-downcase-equivalent accentuation-alist))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(string (cdr pair)) %special-chars)
                          ,(string (car pair))))
                 (add-downcase-equivalent special-chars-alist))
        ,key-code)))

(add-language "Dansk (Danish)" :da
              ((e . è) (o . ò) (a . â)
               (e . é) (o . ó)
               (e . ê) (o . ô)
                       (o . ø))
              ((aa . å) (ae . æ)))

(add-language "Deutsch (German)" :de
              ((a . ä) (e . ë) (i . ï) (o . ö) (u . ü))
              ((ss . ß)))

(add-language "English" :en ())

(add-language "Español (Spanish)" :es
              ((a . á) (e . é) (i . í) (o . ó) (u . ú)
               (n . ñ)         (i . ï)         (u . ü)))

(add-language "Esperanto" :eo
              ((c . ĉ) (g . ĝ) (h . ĥ) (j . ĵ) (s . ŝ)
               (u . ŭ)))

(add-language "Français (French)" :fr
              ((a . â) (e . ê) (i . î) (o . ô) (u . û)
               (a . à) (e . è)                 (u . ù)
                       (e . é)
                       (e . ë) (i . ï)         (u . ü)
               (c . ç) (y . ÿ) (i . î))
              ((oe . œ) (ae . æ)))

(add-language "Italiano (Italian)" :it
              ((e . è) (o . ò) (i . î)
               (e . é) (o . ó)))

(add-language "Norsk (Norwegian)" :no
              ((e . è) (o . ò) (a . â)
               (e . é) (o . ó)
               (e . ê) (o . ô)
                       (o . ø))
              ((aa . å) (ae . æ)))

(add-language "Português (Portuguese)" :pt
              ((a . á) (e . é) (i . í) (o . ó) (u . ú)
               (a . â) (e . ê)         (o . ô) (u . ü)
               (a . à)         (c . ç) (o . õ)
               (a . ã)))

(add-language "Rumàntsch (Romansh)" :rm
              ((e . é) (i . ï) (o . ö) (u . ü)
               (e . è)
               (e . ê)))

(add-language "Suomi (Finnish)" :fi
              ((a . ä) (o . ö) (u . ü)))

(add-language "Svenska (Swedish)" :sv
              ((a . ä) (o . ö) (u . ü))
              ((aa . å)))

(defun remove-accentuation (string)
  "Removes accentuation (according to %ACCENTUATIONS) from STRING."
  (maphash (lambda (k v)
             (setf string (ppcre:regex-replace-all k string v)))
          %accentuations)
  string)

(defun remove-special-chars (string)
  "Removes all special characters stored in %SPECIAL-CHARS using #'PPCRE:REPLACE-REGEX-ALL."
  (maphash (lambda (k v)
             (setf string (ppcre:regex-replace-all k string v)))
           %special-chars)
  string)

(defun remove-ponctuation (string)
  "Removes ponctuation and other special characters from STRING according #'ALPHANUMERICP."
  (labels ((substitute-ponctuation-by-separator (string)
             "If a (alphanumericp char) returns false, such char is replaced with *SLUG-SEPARATOR*."
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

(defun asciify (string)
  "Removes the accentuation and ponctuation of the given STRING."
  (remove-accentuation (remove-special-chars string)))

(defun slugify (string)
  "Makes STRING a slug: a downcase string, with no special characters, ponctuation or accentuated letters whatsoever."
  (remove-accentuation
   (string-downcase
    (remove-special-chars
     (remove-ponctuation string)))))

(defun CamelCaseFy (string)
  "Makes STRING CamelCase, also removing ponctuation and accentuation."
  (remove *slug-separator*
          (asciify (string-capitalize (remove-ponctuation string)))))

(defun smallCamelCaseFy (string)
  "Makes STRING smallCamelCase, also removing ponctuation and accentuation.")

(defun snakefy (string)
  "Makes STRING snake_case, also removing ponctuation and accentuation."
  (let ((*slug-separator* #\_))
    (slugify string)))
