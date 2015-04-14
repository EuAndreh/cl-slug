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

(defmacro aif2 (test then &optional else)
  `(multiple-value-bind (it win) ,test
     (if win
         ,then
          ,else)))

(defparameter *available-languages* ()
  "Alist with (KEY . LANGUAGE-NAMESTRING).")

(defparameter *slug-separator* #\- "Default separator of slug strings.")

(defparameter %accentuations (make-hash-table)
  "Hash table of all {accentuation -> ascii-equivalent} characters.")

(defparameter %special-chars (make-hash-table)
  "Hash table of all {special-char -> ascii-equivalent} strings.")

(defmacro add-language (name key-code accentuation-alist
                        &optional special-chars-alist)
  (flet ((add-upcase-chars (alist)
           "Adds an equivalent upcase character cons pair to every cons pair."
           (remove-duplicates
            (append alist (mapcar (lambda (pair)
                                    (cons (char-upcase (car pair))
                                          (char-upcase (cdr pair))))
                                  alist))))
         (add-upcase-string (alist)
           "Adds an equivalent upcase string cons pair to every string cons pair that has a second element that changes from downcase to upcase."
           (remove-duplicates
            (append alist (mapcar (lambda (pair)
                                    (cons (string-upcase (car pair))
                                          (string-upcase (cdr pair))))
                                  alist))
             :test #'string= :key #'cdr :from-end t)))
    `(progn
       (pushnew (cons ,key-code ,name) *available-languages* :key #'car)
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(cdr pair) %accentuations) ,(car pair)))
                 (add-upcase-chars accentuation-alist))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(cdr pair) %special-chars) ,(car pair)))
                 (add-upcase-string special-chars-alist))
        ,key-code)))
;; merge hash???

(add-language "Dansk (Danish)" :da
              ((#\e . #\è) (#\o . #\ò) (#\a . #\â)
               (#\e . #\é) (#\o . #\ó)
               (#\e . #\ê) (#\o . #\ô)
                           (#\o . #\ø))
              (("aa" . "å") ("ae" . "æ")))

(add-language "Deutsch (German)" :de
              ((#\a . #\ä) (#\e . #\ë) (#\i . #\ï) (#\o . #\ö) (#\u . #\ü))
              (("ss" . "ß")))

(add-language "English" :en ())

(add-language "Español (Spanish)" :es
              ((#\a . #\á) (#\e . #\é) (#\i . #\í) (#\o . #\ó) (#\u . #\ú)
               (#\n . #\ñ)             (#\i . #\ï)             (#\u . #\ü)))

(add-language "Esperanto" :eo
              ((#\c . #\ĉ) (#\g . #\ĝ) (#\h . #\ĥ) (#\j . #\ĵ) (#\s . #\ŝ)
               (#\u . #\ŭ)))

(add-language "Français (French)" :fr
              ((#\a . #\â) (#\e . #\ê) (#\i . #\î) (#\o . #\ô) (#\u . #\û)
               (#\a . #\à) (#\e . #\è)                         (#\u . #\ù)
                           (#\e . #\é)
                           (#\e . #\ë) (#\i . #\ï)             (#\u . #\ü)
               (#\c . #\ç) (#\y . #\ÿ) (#\i . #\î))
              (("oe" . "œ") ("ae" . "æ")))

(add-language "Italiano (Italian)" :it
              ((#\e . #\è) (#\o . #\ò) (#\i . #\î)
               (#\e . #\é) (#\o . #\ó)))

(add-language "Norsk (Norwegian)" :no
              ((#\e . #\è) (#\o . #\ò) (#\a . #\â)
               (#\e . #\é) (#\o . #\ó)
               (#\e . #\ê) (#\o . #\ô)
                           (#\o . #\ø))
              (("aa" . "å") ("ae" . "æ")))

(add-language "Português (Portuguese)" :pt
              ((#\a . #\á) (#\e . #\é) (#\i . #\í) (#\o . #\ó) (#\u . #\ú)
               (#\a . #\â) (#\e . #\ê)             (#\o . #\ô) (#\u . #\ü)
               (#\a . #\à)             (#\c . #\ç) (#\o . #\õ)
               (#\a . #\ã)))

(add-language "Rumàntsch (Romansh)" :rm
              ((#\e . #\é) (#\i . #\ï) (#\o . #\ö) (#\u . #\ü)
               (#\e . #\è)
               (#\e . #\ê)))

(add-language "Suomi (Finnish)" :fi
              ((#\a . #\ä) (#\o . #\ö) (#\u . #\ü)))

(add-language "Svenska (Swedish)" :sv
              ((#\a . #\ä) (#\o . #\ö) (#\u . #\ü))
              (("aa" . "å")))

(defun remove-accentuation (string)
  "Removes accentuation (according to %ACCENTUATIONS) from STRING."
  (map 'string (lambda (char)
                 (aif2 (gethash char %accentuations)
                       it
                       char))
       string))

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

(defun remove-special-chars (string)
  "Removes all special characters stored in %SPECIAL-CHARS using #'PPCRE:REPLACE-REGEX-ALL."
  (maphash (lambda (k v)
             (setf string (ppcre:regex-replace-all k string v)))
           %special-chars)
  string)

(defun asciify (string)
  "Removes the accentuation and ponctuation of the given STRING."
  (remove-accentuation (remove-special-chars string)))

(defun slugify (string)
  "Makes STRING a slug: a downcase string, with no special characters, ponctuation or accentuated letters whatsoever, according to the chosen CHARSET."
  (remove-accentuation
   (string-downcase
    (remove-special-chars
     (remove-ponctuation string)))))

(defun CamelCaseFy (string)
  "Makes STRING CamelCase, also removing ponctuation and accentuation, according to the chosen CHARSET."
  (remove *slug-separator*
          (asciify (string-capitalize (remove-ponctuation string)))))

(defun snakefy (string)
  "Makes STRING snake_case, also removing ponctuation and accentuation, according to the chosen CHARSET."
  (let ((*slug-separator* #\_))
    (slugify string)))
