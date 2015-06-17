# cl-slug
[![Build Status](https://travis-ci.org/EuAndreh/cl-slug.svg?branch=master)](https://travis-ci.org/EuAndreh/cl-slug)
[![Circle CI](https://circleci.com/gh/EuAndreh/cl-slug.svg?style=svg)](https://circleci.com/gh/EuAndreh/cl-slug)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/cl-slug/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/cl-slug?branch=master)

Easily create slugs from any string. Supports many languages alphabets. See [Supported languages](#supported-languages) to check for supported languages or to help to add one.

Inspired by [Lisp Web Tales](http://lispwebtales.ppenev.com/chap05.html#leanpub-auto-rewriting-the-routes).

## Usage
```lisp
* (ql:quickload :cl-slug)
; => (:CL-SLUG)
* (import '(slug:slugify slug:asciify slug:CamelCaseFy) 
; => T
```

The main function is called `slugify`:

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

Or if you want a CamelCase, use `CamelCaseFy`:

```lisp
* (CamelCaseFy "My new camel case string")
; => "MyNewCamelCaseString"
```

## Dependencies

This library depends on [CL-PPCRE](http://weitz.de/cl-ppcre). The test package uses the [prove](https://github.com/fukamachi/prove) test library.

## Installation
Available on [Quicklisp](http://quicklisp.org):
```
(ql:quickload :cl-slug)
```

## Supported languages
The languages that are supported right now are:
- english
- portuguese
- esperanto
- german
- french
- swedish
- finnish
- norwegian
- danish
- italian
- spanish
- romansh

Ported from [Django](https://code.djangoproject.com/browser/django/trunk/django/contrib/admin/media/js/urlify.js)():
- currency
- romanian
- lithuanian
- latvian
- polish
- czesh
- ukranian
- russian
- turkish
- greek
- latin

At the present moment, adding new languages is a fairly manual process:
  1. Identify non-ASCII characters in a given language's alphabet
  2. Stablish equivalence between the found characters and ASCII characters
  3. Write them down in the code.

All those things can actually be done for most of the dominant western languages, but can't be applied for minor regional languages or many other non-latin languages from the whole world, like chinese. It's not generic and not scalable.

I couldn't think of a solution so far for this, but if you know a solution (even a partial one) I'd be glad to hear =].

## Bugs
If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests
This library is tested under [ABCL](https://common-lisp.net/project/armedbear/), [SBCL](http://www.sbcl.org/), [CCL](http://ccl.clozure.com/), [CLISP](http://www.clisp.org/) and [ECL](https://common-lisp.net/project/ecl/) Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :cl-slug)
; prints lots of (colorful) stuff...
; => T
```

Tests are ran with [Travis CI](https://travis-ci.org/EuAndreh/cl-slug) and [Circle CI](https://circleci.com/gh/EuAndreh/cl-slug) using [cl-travis](https://github.com/luismbo/cl-travis), [CIM](https://github.com/KeenS/CIM), [cl-coveralls](https://github.com/fukamachi/cl-coveralls) and [Roswell](https://github.com/snmsts/roswell). Check it out!

## Author
André Miranda

## License
[LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).
