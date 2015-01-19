(in-package cl-user)
(defpackage cl-slug
  (:use cl)
  (:export slugify
           *slug-separator*)
  (:documentation "Main (and only) package."))
(in-package cl-slug)

(defparameter *available-languages* ()
  "Alist with (`key' . `language-namestring').")

(defvar *special-chars-alist* ()
  "Special var to hold alist with special chars that behave differently from others.")

(defvar *accentuation-alist* ()
  "Special var to hold alist with accentuated chars and their respective accentless char.")

(defparameter *slug-separator* #\- "Default separator of slug strings.")

(defparameter %langname->accentuation-alist (make-hash-table)
  "Hash table that holds the alist of accentuation from each language indexed by key.")

(defparameter %langname->special-chars-alist (make-hash-table)
  "Hash table that holds the alist of special characters from each language indexed by key.")

(defmacro add-language (name key-code accentuation-alist
                        &optional special-chars-alist)
  "Adds a language to both hash tables (`%langname->accentuation-alist' and `%langname->special-chars-alist') and the language `name' + `key-code' to `*available-languages*'. Also adds all new special and accentuated chars to the `:all' entry on both hash tables."
  (flet ((add-upcase-char (alist)
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
       (setf (gethash ,key-code %langname->accentuation-alist)
             ',accentuation-alist
             (gethash ,key-code %langname->special-chars-alist)
             ',special-chars-alist)
       ,@(mapcar (lambda (pair)
                   `(pushnew ',pair (gethash :all %langname->accentuation-alist)
                             :key #'cdr))
                 (add-upcase-char accentuation-alist))
       ,@(mapcar (lambda (pair)
                   `(pushnew ',pair (gethash :all %langname->special-chars-alist)
                             :key #'cdr))
                 (add-upcase-string special-chars-alist))
       ,key-code)))

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

(defun slugify (string &optional (charset :en))
  "Makes STRING a slug: a downcase string, with no special characters, ponctuation or accentuated letters whatsoever."
  (let ((*accentuation-alist*
         (multiple-value-bind (it win)
             (gethash charset %langname->accentuation-alist)
           (if win
               it
               (error "Invalid charset option: ~S" charset))))
        (*special-chars-alist* (gethash charset %langname->special-chars-alist)))
    (remove-accentuation
     (string-downcase
      (remove-special-chars
       (remove-ponctuation string))))))
