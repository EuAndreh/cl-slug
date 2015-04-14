# cl-slug

[![Build Status](https://travis-ci.org/EuAndreh/cl-slug.svg?branch=master)](https://travis-ci.org/EuAndreh/cl-slug)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/cl-slug/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/cl-slug?branch=master)

Easily create slugs from any string. Supports many languages alphabets. See [Supported languages](#supported-languages) to check for supported languages or to help to add one.

Inspired by [Lisp Web Tales](http://lispwebtales.ppenev.com/chap05.html#leanpub-auto-rewriting-the-routes).

## Usage

```lisp
* (ql:quickload :cl-slug)
; => (:CL-SLUG)
* (import '(slug:slugify slug:asciify slug:CamelCaseFy slug:snakefy))
; => T
```

The main (and only) function is called `slugify`:

```lisp
* (slugify "My new cool article, for the blog (V. 2).")
; => "my-new-cool-article-for-the-blog-v-2"
* (slugify "André Miranda")
; => "andre-miranda"
```

`slugify` removes any accentuated character, replacing it with an unaccentuated equivalent, and any ponctuation (a ponctuation is a char that returns `NIL` for `alphanumericp`) and puts a dash (`-`) on it's place. You can change that by binding (of `setf`ing) `*slug-separator*`:

```lisp
* (let ((*slug-separator* #\/))
    (slugify "Testing the *slug-separator* var..."))
; => "testing_the_slug_separator_var"
```

To that specific case, you can use `snakefy`:
```lisp
* (snakefy "Using now snakefy")
; => "using_now_snakefy"
```

`slugify` also ignores numbers:

```lisp
* (slugify "one2three4five")
; => "one2three4five"
```

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

## Dependencies

This library depends on [CL-PPCRE](http://weitz.de/cl-ppcre). The test package uses the [prove](https://github.com/fukamachi/prove) test library.

## Installation

Now available on [Quicklisp](http://quicklisp.org)! Just one `(ql:quickload :cl-slug)` away!

## Supported languages

The languages that are supported right now are english, portuguese, esperanto, german, french, swedish, finnish, norwegian, danish, italian, spanish and romansh strings.

   At the present moment, adding new languages is a fairly manual process:
   1. Identify non-ASCII characters in a given language's alphabet
   2. Stablish equivalence between the found characters and ASCII characters
   3. Write them down in the code.

   All those things can actually be done for most of the dominant western languages, but can't be applied for minor regional languages or many other non-latin languages from the whole world, like chinese. I couldn't think of a solution so far for this, but if anyone knows a solution (even a partial one) I'd be glad to hear =].

## Bugs

If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests

This library is tested under SBCL, CCL and CLISP Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :cl-slug)
; prints lots of (colorful) stuff...
; => T
```

Tests are also ran with [Travis CI](https://travis-ci.org/EuAndreh/cl-slug) using [cl-travis](https://github.com/luismbo/cl-travis) and [CIM](https://github.com/KeenS/CIM). Check it out!

## Author

André Miranda

## License

Licensed under the [LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext) License.

[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/EuAndreh/cl-slug/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
