==================
 ample-regexps.el
==================

Ample regular expressions — Compose and reuse Emacs regular expressions with
ease.

If you ever tried to write more than a few related regexps and it felt that
there should be a way to pick out their common parts and just plug them in
without worrying about grouping and precedence, this package is for you.

Installation
------------

``ample-regexps`` is available on `MELPA <http://melpa.milkbox.net>`_ from where it
can be installed via::

    M-x package-install ample-regexps

If you haven't yet added MELPA repositories to your config, feel free to follow
`these instructions <http://melpa.milkbox.net/#/getting-started>`_ to do so.

Also, since this package has no dependencies, you can just drop the
``ample-regexps.el`` file somewhere on ``load-path`` and enable it via

.. code-block:: emacs-lisp

   (require 'ample-regexps)

``ample-regexps`` is tested to work on Emacs24.  It *should* work on Emacs23,
but no guarantees about that.

Contributing
------------

There's plenty of ways to help: use this package, spread the word, fix bugs,
post bug reports or fresh ideas to the issue tracker, add tests, etc.

To participate in development, you'll probably need `cask
<https://github.com/cask/cask>`_.  The only dependency as of now is
`ert-runner`, so it's possible to run tests manually, but it's rather
inconvenient.  It's a lot easier to just do:

.. code-block:: bash

   $ cask install
   $ make test

Documentation
-------------

Basic Usage
===========

The main item of the API is the ``define-arx`` macro.  Let's start with a simple
example:

.. code-block:: emacs-lisp

    (define-arx hello-world-rx '()) ;; -> hello-world-rx

    (hello-world-rx "Hello, world") ;; -> "Hello, world"

    (hello-world-rx (* "Hello, world")) ;; -> "\\(?:Hello, world\\)*"

``define-arx`` defines a macro that converts s-exps into regular expressions.
If you're familiar with `rx
<http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/rx.el>`_
package — if not, I encourage you to do so — you're probably starting to
experience déjà vu.  You're right: ``rx`` *is* used underneath,
``ample-regexps`` is just a cherry on the pie adding customization with a hint
of syntactic sugar atop.

Aliasing
========

Let's start with something simple and see how you can alias components to save
some keystrokes:

.. code-block:: emacs-lisp

    (define-arx h-w-rx
      '((h "Hello, ")
        (w "world"))) ;; -> hello-world-rx

    (h-w-rx h w) ;; -> "Hello, world"

    (h-w-rx (* h w)) ;; -> "\\(?:Hello, world\\)*"

Aliased literals are regexp quoted, but you can alias a regular expression if
you want:

.. code-block:: emacs-lisp

    (define-arx alnum-rx
      '((alpha_ (regexp "[[:alpha:]_]"))
        (alnum_ (regexp "[[:alnum:]_]")))) ;; -> alnum-rx

    (alnum-rx (+ alpha_) (* alnum_)) ;; -> "[[:alpha:]_]+[[:alnum:]_]*"

In fact, ``(regexp ...)`` is just an ``rx`` S-expression which you can compose
and nest arbitrarily to define even more forms:

.. code-block:: emacs-lisp

    (define-arx assignment-rx
      '((alpha_ (regexp "[[:alpha:]_]"))
        (alnum_ (regexp "[[:alnum:]_]"))
        (ws (* blank))
        (id (seq symbol-start (+ alpha_) (* alnum_) symbol-end)))) ;; -> assignment-rx

    (assignment-rx id ws "=" ws id) ;; -> "\\_<[[:alpha:]_]+[[:alnum:]_]*\\_>[[:blank:]]*=[[:blank:]]*\\_<[[:alpha:]_]+[[:alnum:]_]*\\_>"

Custom S-expressions
====================

Ok, this was all simple aliasing, but what if you want to add some custom
S-expressions, too?  Fear thou not, we've got you covered:

.. code-block:: emacs-lisp

    (define-arx cond-assignment-rx
      '((alpha_ (regexp "[[:alpha:]_]"))
        (alnum_ (regexp "[[:alnum:]_]"))
        (ws (* blank))
        (sym (:func (lambda (_form &rest args)
                      `(seq symbol-start (or ,@args) symbol-end))))
        (cond-keyword (sym "if" "elif" "while"))
        (id (sym (+ alpha_) (* alnum_))))) ;; -> cond-assignment-rx

    (cond-assignment-rx cond-keyword ws id ":" id ws "=" ws id) ;; -> "\\_<\\(?:elif\\|if\\|while\\)\\_>[[:blank:]]*\\_<\\(?:[[:alpha:]_]+\\|[[:alnum:]_]*\\)\\_>:\\_<\\(?:[[:alpha:]_]+\\|[[:alnum:]_]*\\)\\_>[[:blank:]]*=[[:blank:]]*\\_<\\(?:[[:alpha:]_]+\\|[[:alnum:]_]*\\)\\_>"

``(:func ...)`` plist allows to use a simple function that will be passed all the
s-expressions from the form as arguments with the first argument will being the
form symbol itself.  You can treat them as a list like above or decompose and
name to your liking (``destructuring-bind`` anyone?).  Let's see how one could
write a matcher for a list of comma-separated values:

.. code-block:: emacs-lisp

    (define-arx csv-rx
      '((csv (:func (lambda (_form n arg)
                      `(seq ,@(nbutlast (cl-loop for i from 1 to n
                                                 collect `(group-n ,i ,arg)
                                                 collect ", ")))))))) ;; -> csv-rx

    (csv-rx (csv 3 (seq "foobar"))) ;; -> "\\(?1:foobar\\), \\(?2:foobar\\), \\(?3:foobar\\)"

There's a drawback to this, if you pass an incorrect number of arguments,
you'll get an unreadable error message:

.. code-block:: emacs-lisp

    (csv-rx (csv 3 "foo" "bar")) ;; -> Wrong number of arguments: (lambda (_form n arg) (\` (seq (\,@ (nbutlast (cl-loop for i from 1 to n collect (\` (group-n (\, i) (\, arg))) collect ", ")))))), 4

To make this more readable, form-function plist supports ``:min-args`` and ``:max-args`` keywords:

.. code-block:: emacs-lisp

    (define-arx csv-rx
      '((csv (:func (lambda (_form n arg)
                      `(seq ,@(nbutlast (cl-loop for i from 1 to n
                                                 collect `(group-n ,i ,arg)
                                                 collect ", "))))
                    :min-args 2
                    :max-args 2)))) ;; -> csv-rx

    (csv-rx (csv 3 "foo" "bar")) ;; -> (error "rx form `csv' accepts at most 2 args")

    (csv-rx (csv 3)) ;; -> (error "rx form `csv' requires at least 2 args")

Recursion
=========

Form functions obviously can be made to support recursion.  You may have
noticed that ``csv-rx`` only matches lists of exactly N elements.  Let's fix it
to match any length up to N (you can achieve the same effect with a simple
loop, but I really wanted to avoid using factorial to show recursion):

.. code-block:: emacs-lisp

    (defun csv-opt (_form n elt &optional accum)
      (cond
       ((<= n 0) accum)
       ((null accum) (list _form (1- n) elt (list 'group-n n elt)))
       (t (list _form (1- n) elt (list 'group-n n elt `(opt ", " ,accum)))))) ;; -> csv-opt

    (define-arx csv-opt-rx
      '((csv-opt (:func csv-opt)))) ;; -> csv-opt-rx

    (csv-opt-rx (csv-opt 3 "foo")) ;; -> "\\(?1:foo\\(?:, \\(?2:foo\\(?:, \\(?3:foo\\)\\)?\\)\\)?\\)"

Such expressions in plain-text are hardly readable, let alone maintainable, but
wrapped in a function call they don't seem scary at all.

Raw Power
=========

Form functions can return raw regular expressions, too.  This is, for example,
how you could backport ``group-n`` form to Emacs23 where it's not available (if
you had to):

.. code-block:: emacs-lisp

    (define-arx backport-rx
      '((group-n (:func (lambda (_form index &rest args)
                          (concat (format "\\(?%d:" index)
                                  (mapconcat (lambda (f) (rx-form f ':)) args "")
                                  "\\)")))))) ;; -> backport-rx

    (backport-rx (group-n 1 (seq "foo" (* "bar")))) ;; -> "\\(?1:foo\\(?:bar\\)*\\)"

The snippet above uses ``mapconcat`` and a bit of underdocumented ``rx``
functionality, you can avoid that with special convenience functions:
``arx-and`` and ``arx-or``:

.. code-block:: emacs-lisp

    (define-arx backport-rx
      '((group-n (:func (lambda (_form index &rest args)
                          (concat (format "\\(?%d:" index)
                                  (arx-and args)
                                  "\\)")))))) ;; -> backport-rx

    (backport-rx (group-n 1 (seq "foo" (* "bar")))) ;; -> "\\(?1:foo\\(?:bar\\)*\\)"

Be warned though, this is a power user feature and no extra grouping will be
performed which may cause unexpected results:

.. code-block:: emacs-lisp

    (define-arx ungrouped-rx
      '((foo (:func (lambda (_form) "foo"))))) ;; -> ungrouped-rx

    (ungrouped-rx (foo) (foo)) ;; -> "foofoo"

    (ungrouped-rx (* (foo))) ;; -> "foo*"

To avoid surprises, make sure you the resulting expressions are grouped.

How Does This Work
==================

``(define-arx foobar-rx ...)`` is a macro, that defines three things:

- a macro ``(foobar-rx ...)`` to be replaced by a constant during compilation
- a function ``(foobar-rx-to-string ...)`` that can be used in runtime
- a variable ``foobar-rx-constituents`` with form definitions to use

When either the function or the macro is called, constituents variable is used
to override ``rx-constituents`` via dynamic scoping and the rest is performed by
``rx-to-string`` function.

License
-------

This package is provided under the terms and conditions of GPLv3 license.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/ .
