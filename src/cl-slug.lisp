(defpackage cl-slug
  (:use cl)
  (:nicknames slug)
  (:export slugify
           asciify
           CamelCaseFy
           *available-languages*
           *slug-separator*)
  (:documentation "Main (and only) package. Package nickname SLUG also available."))
(in-package cl-slug)

(defparameter *available-languages* ()
  "Alist with (KEY . LANGUAGE-NAMESTRING).")

(defparameter *slug-separator* #\- "Default separator of slug strings.")

(defparameter %accentuations (make-hash-table :test #'equal)
  "Hash table of all {accentuation -> ascii-equivalent} characters.")

(defparameter %special-chars (make-hash-table :test #'equal)
  "Hash table of all {special-char -> ascii-equivalent} strings.")

(defmacro add-language (name key-code accentuation-alist
                        &optional special-chars-alist)
  (flet ((add-upcase-equivalent (alist)
           "Adds an equivalent downcase cons pair to every cons pair."
           (remove-duplicates
            (append (mapcar (lambda (pair)
                              (cons (string-upcase (princ-to-string (car pair)))
                                    (string-upcase (princ-to-string (cdr pair)))))
                            alist)
                    (mapcar (lambda (pair)
                              (cons (string-downcase (princ-to-string (car pair)))
                                    (string-downcase (princ-to-string (cdr pair)))))
                            alist)))))
    `(progn
       (pushnew (cons ,key-code ,name) *available-languages* :key #'car)
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(cdr pair) %accentuations)
                          ,(car pair)))
                 (add-upcase-equivalent accentuation-alist))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ,(cdr pair) %special-chars)
                          ,(car pair)))
                 (add-upcase-equivalent special-chars-alist))
        ,key-code)))

(add-language "Currency" :currency
              ()
              ((indian-rupee . ₹) (dollar . \\$) ;; This confuses the regex => unwanted
               (baht . ฿) (currency . ¤) (ecu . ₠) (rial . ﷼)
               (yen . 円) (yuan . 元) (yen . ¥) (cent . ¢) (cedi . ₵) (hryvnia . ₴) (austral . ₳)
               (guarani . ₲) (peso . ₱) (penny . ₰) (drachma . ₯) (tugrik . ₮) (kip . ₭) (dong . ₫)
               (new-shequel . ₪) (won . ₩) (rupee . #\₨) ;; The ₨ symbols's string is RS => unwanted
               (peseta . ₧) (naira . ₦) (mill . ₥)
               (lira . ₤) (pound . £) (french-franc . ₣) (cruzeiro . ₢) (euro . €)))

(add-language "Čeština (Czech)" :cs
              ((z . ž) (u . ů) (t . ť) (s . š) (r . ř) (n . ň) (e . ě) (d . ď) (c . č)))

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

(add-language "ελληνικά (Greek)" :el
              ((i . ΐ) (y . ϋ) (y . ΰ) (i . ϊ) (s . ς) (w . ώ) (h . ή) (y . ύ) (o . ό)
               (i . ί) (e . έ) (a . ά) (w . ω) (x . χ) (f . φ) (y . υ) (t . τ) (s . σ)
               (r . ρ) (p . π) (o . ο) (3 . ξ) (n . ν) (m . μ) (l . λ) (k . κ) (i . ι)
               (8 . θ) (h . η) (z . ζ) (e . ε) (d . δ) (g . γ) (b . β) (a . α))
              ((ps . ψ)))

(add-language "Íslenska (Icelandic)" :is
              ((a . á) (e . é) (i . í) (o . ó) (u . ú) (y . ý) (o . ö))
              ((d . ð) (th . þ) (ae . æ)))

(add-language "Italiano (Italian)" :it
              ((e . è) (o . ò) (i . î)
               (e . é) (o . ó)))

(add-language "Lingua Latīna (Latin)" :la
              ((y . ÿ) (y . ý) (u . ű) (u . ü) (u . û) (u . ú) (u . ù) (o . ø) (o . ő)
               (o . ö) (o . õ) (o . ô) (o . ó) (o . ò) (n . ñ) (d . ð) (i . ï) (i . î)
               (i . í) (i . ì) (e . ë) (e . ê) (e . é) (e . è) (c . ç) (a . å) (a . ä)
               (a . ã) (a . â) (a . á) (a . à))
              ((ss . ẞ) (ss . ß) (th . þ) (ae . æ)))

(add-language "Latviešu (Latvian)" :lv
              ((u . ū) (n . ņ) (l . ļ) (k . ķ) (i . ī) (g . ģ) (e . ē) (a . ā)))

(add-language "Lietuvių (Lithuanian)" :lt
              ((u . ų) (i . į) (e . ė)))

(add-language "Norsk (Norwegian)" :no
              ((e . è) (o . ò) (a . â)
               (e . é) (o . ó)
               (e . ê) (o . ô)
                       (o . ø))
              ((aa . å) (ae . æ)))

(add-language "Polski (Polish)" :pl
              ((z . ż) (z . ź) (s . ś) (n . ń) (l . ł) (e . ę) (c . ć) (a . ą)))

(add-language "Português (Portuguese)" :pt
              ((a . á) (e . é) (i . í) (o . ó) (u . ú)
               (a . â) (e . ê)         (o . ô) (u . ü)
               (a . à)         (c . ç) (o . õ)
               (a . ã)))

(add-language "Rumàntsch (Romansh)" :rm
              ((e . é) (i . ï) (o . ö) (u . ü)
               (e . è)
               (e . ê)))

(add-language "Română (Romanian)" :ro
              ((a . ă) (s . ș) (t . ţ) (t . ț)))

(add-language "Ру́сский (Russian)" :ru
              ((e . э) (y . ы) (u . ъ) (c . ц) (h . х) (f . ф) (u . у) (t . т) (s . с)
               (r . р) (p . п) (o . о) (n . н) (m . м) (l . л) (k . к) (j . й) (i . и)
               (z . з) (e . е) (d . д) (g . г) (v . в) (b . б) (a . а) ("" . ь))
              ((ya . я) (yu . ю) (sh . щ) (sh . ш) (ch . ч) (zh . ж) (yo . ё)))

(add-language "українська (Ukrainian)" :uk
              ((g . ґ) (yi . ї) (i . і) (ye . є)))

(add-language "Беларуская (Belarusian)" :be
              ((w . ў)))

(add-language "Suomi (Finnish)" :fi
              ((a . ä) (o . ö) (u . ü)))

(add-language "Svenska (Swedish)" :sv
              ((a . ä) (o . ö) (u . ü))
              ((aa . å)))

(add-language "Türkçe (Turkish)" :tr
              ((g . ğ) (i . İ) (i . ı) (s . ş)))


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

(defun last-char (string)
  "Returns the last char of a non-empty string."
  (let ((index (1- (length string))))
    (when (not (= -1 index))
      (elt string index))))

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
             (reduce (lambda (string char)
                       (let ((last-char (last-char string)))
                         (if (and last-char
                                  (char= char *slug-separator* (last-char string)))
                             string
                             (concatenate 'string
                                          string
                                          (string char)))))
                     string
                     :initial-value "")))
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

(defun camelcasefy (string)
  "Makes STRING CamelCase, also removing ponctuation and accentuation."
  (remove *slug-separator*
          (asciify (string-capitalize (remove-ponctuation string)))))
