cl-slug
=======

[![Build Status](https://travis-ci.org/EuAndreh/cl-slug.svg?branch=master)](https://travis-ci.org/EuAndreh/cl-slug)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/cl-slug/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/cl-slug?branch=master)

Easily create slugs from any string. Supports many languages alphabets. See [Supported languages](#supported-languages) to check for supported languages or to help to add one.

Inspired by [Lisp Web Tales](http://lispwebtales.ppenev.com/chap05.html#leanpub-auto-rewriting-the-routes).

Usage
-----

```lisp
* (ql:quickload :cl-slug)
; => (:CL-SLUG)
* (import '(cl-slug:slugify cl-slug:asciify cl-slug:CamelCaseFy))
; => T
```

The main (and only) function is called `slugify`:

```lisp
* (slugify "My new cool article, for the blog (V. 2).")
; => "my-new-cool-article-for-the-blog-v-2"
* (slugify "André Miranda" :pt)
; => "andre-miranda"
```

`slugify` removes any accentuated character, replacing it with an unaccentuated equivalent, and any ponctuation (a ponctuation is a char that returns `NIL` for `alphanumericp`) and puts a dash (`-`) on it's place. You can change that by binding (of `setf`ing) `*slug-separator*`:

```lisp
* (let ((*slug-separator* #\_))
    (slugify "Testing the *slug-separator* var..."))
; => "testing_the_slug_separator_var"
```

`slugify` also ignores numbers:

```lisp
* (slugify "one2three4five")
; => "one2three4five"
```

`slugify` by default looks only for ponctuation characters. If you want to use it with any of the supported languages, specify it on the `charset` optional parameter:

```lisp
* (slugify "My string with esperanto language characters (ĉ, and ŭ)" :eo)
; => "my-string-with-esperanto-language-characters-c-and-u"
* (slugify "My string with swedish language characters (ä, ö, ü and å)" :sv)
; => "my-string-with-swedish-language-characters-a-o-u-and-aa"
```

If you want to use `slugify` (for some crazy reason) to create slugs from multi-language strings, use the `:all` option.

If you just want to remove accentuation and ponctuation of a given string, use `asciify`:

```lisp
* (asciify "Eu André!" :pt)
; => "Eu Andre!"
```

Or if you want a CamelCase string, use `CamelCaseFy`:

```lisp
* (CamelCaseFy "My new camel case string")
; => "MyNewCamelCaseString"
```

If you want an ASCII CamelCase string, just compose `asciify` with `CamelCaseFy` =].

Dependencies
------------

This library depends on the [CL-PPCRE](http://weitz.de/cl-ppcre) and on [SPLIT-SEQUENCE](http://www.cliki.net/split-sequence). The test package uses the [prove](https://github.com/fukamachi/prove) test library.

Installation
------------

Now available on [Quicklisp](http://quicklisp.org)! Just one `(ql:quickload :cl-slug)` away!

Supported languages
-------------------

The languages that are supported right now are english, portuguese, esperanto, german, french, swedish, finnish, norwegian, danish, italian, spanish and romansh strings.

   At the present moment, adding new languages is a fairly manual process:
   1. Identify non-ASCII characters in a given language's alphabet
   2. Stablish equivalence between the found characters and ASCII characters
   3. Write them down in the code.

   All those things can actually be done for most of the dominant western languages, but can't be applied for minor regional languages or many other non-latin languages from the whole world, like chinese. I couldn't think of a solution so far for this, but if anyone knows a solution (even a partial one) I'd be glad to hear =].

Bugs
----

If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

Tests
-----

This library is tested under SBCL, CCL and CLISP Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :cl-slug)
; prints lots of stuff...
; => T
```

Tests are also ran with [Travis CI](https://travis-ci.org/EuAndreh/cl-slug) using [cl-travis](https://github.com/luismbo/cl-travis) and [CIM](https://github.com/KeenS/CIM). Check it out!

Author
------

André Miranda

License
-------

Licensed under the LLGPL License.


