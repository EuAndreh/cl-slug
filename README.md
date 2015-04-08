cl-slug
=======

[<https://travis-ci.org/EuAndreh/cl-slug.svg?branch=master>][] [<https://coveralls.io/repos/EuAndreh/cl-slug/badge.svg?branch=master>][]

Easily create slugs from any string. Supports many languages alphabets. See [Supported languages][] to check for supported languages or to help to add one.

Inspired by [Lisp Web Tales][].

Usage
-----

``` commonlisp
* (ql:quickload :cl-slug)
; => (:CL-SLUG)
* (import '(cl-slug:slugify cl-slug:asciify cl-slug:CamelCaseFy))
; => T
```

The main (and only) function is called `slugify`:

``` commonlisp
* (slugify "My new cool article, for the blog (V. 2).")
; => "my-new-cool-article-for-the-blog-v-2"
* (slugify "André Miranda" :pt)
; => "andre-miranda"
```

`slugify` removes any accentuated character, replacing it with an unaccentuated equivalent, and any ponctuation (a ponctuation is a char that returns `NIL` for `alphanumericp`) and puts a dash (`-`) on it's place. You can change that by binding (of `setf=ing) =*slug-separator*`:

``` commonlisp
* (let ((*slug-separator* #\_))
    (slugify "Testing the *slug-separator* var..."))
; => "testing_the_slug_separator_var"
```

`slugify` also ignore numbers:

``` commonlisp
* (slugify "one2three4five")
; => "one2three4five"
```

`slugify` by default looks only for ponctuation characters. If you want to use it with any of the supported languages, specify it on the `charset` optional parameter:

``` commonlisp
* (slugify "My string with esperanto language characters (ĉ, and ŭ)" :eo)
; => "my-string-with-esperanto-language-characters-c-and-u"
* (slugify "My string with swedish language characters (ä, ö, ü and å)" :sv)
; => "my-string-with-swedish-language-characters-a-o-u-and-aa"
```

If you want to use `slugify` (for some crazy reason) to create slugs from multi-language strings, use the `:all` option.

If you just want to remove accentuation and ponctuation of a given string, use `asciify`:

``` commonlisp
* (asciify "Eu André!" :pt)
; => "Eu Andre!"
```

Or if you want a CamelCase string, use `CamelCaseFy`:

``` commonlisp
* (CamelCaseFy "My new camel case string")
; => "MyNewCamelCaseString"
```

If you want an ASCII CamelCase string, just compose `asciify` with `CamelCaseFy` =].

Dependencies
------------

This library depends on the [CL-PPCRE][] and on [SPLIT-SEQUENCE][]. The test package uses the [prove][] test library.

Installation
------------

Now available on [Quicklisp][]!

Supported languages
-------------------

English, portuguese, esperanto, german, french, swedish, finnish, norwegian, danish, italian, spanish and romansh strings.

Right now, adding new languages is a fairly manual process:
1.  Identify non

  [<https://travis-ci.org/EuAndreh/cl-slug.svg?branch=master>]: https://travis-ci.org/EuAndreh/cl-slug
  [<https://coveralls.io/repos/EuAndreh/cl-slug/badge.svg?branch=master>]: https://coveralls.io/r/EuAndreh/cl-slug
  [Supported languages]: #supported-languages
  [Lisp Web Tales]: http://lispwebtales.ppenev.com/chap05.html#leanpub-auto-rewriting-the-routes
  [CL-PPCRE]: http://weitz.de/cl-ppcre/
  [SPLIT-SEQUENCE]: http://www.cliki.net/split-sequence
  [prove]: http://github.com/fukamachi/prove
  [Quicklisp]: http://quicklisp.org
