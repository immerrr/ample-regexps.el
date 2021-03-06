;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
;; -*- no-byte-compile: t; -*-
(require 'ert)

(ert-deftest arx-empty-list ()
  (with-myrx
   '()
   (should (equal (myrx "foobar") "foobar"))))

(ert-deftest arx-alias-for-literal-basic ()
  (with-myrx
   '((hello "Hello"))
   (should (equal (myrx hello) "Hello"))
   (should (equal (myrx hello ", world") "Hello, world"))
   (should (member (myrx (or hello ", world") )
                   '("\\(?:, world\\|Hello\\)" "Hello\\|, world")))
   (should (equal (myrx (* hello)) "\\(?:Hello\\)*"))))

(ert-deftest arx-constituents-are-redefined ()
  (with-myrx
   '((hello "Hello"))
   (should (equal (myrx-to-string '(: hello) t) "Hello"))

   ;; Ensure this macro is not expanded during ert test definition.
   (eval (quote (define-arx myrx '((foobar "foobar")))))

   (should-error-re
    (myrx-to-string '(: hello) t)
    "Unknown rx \\(form\\|symbol\\) [‘`]hello['’]")))


(ert-deftest arx-alias-for-literal-with-quoting ()
  (with-myrx
   '((foobar "^$")
     (empty-line (regexp "^$")))
   (should (equal (myrx foobar) "\\^\\$"))
   (should (equal (myrx empty-line) "^$"))))


(ert-deftest arx-alias-for-regexp ()
  (with-myrx
   '((ws (regexp "[ \t]*")))
   (should (equal (myrx ws) "[ \t]*"))))


(ert-deftest arx-alias-for-other-symbol ()
  (with-myrx
   '((ws (regexp "[ \t]*"))
     (ws2 ws))
   (should (equal (myrx ws2) "[ \t]*"))))


(ert-deftest arx-alias-for-rx-sexp ()
  (with-myrx
   '((for (seq symbol-start "for" symbol-end)))
   (should (equal (myrx for) "\\_<for\\_>"))))


(ert-deftest arx-alias-for-arx-sexp ()
  (with-myrx
   '((name (regexp "[[:alnum:]_]+"))
     (assign (seq name "=" name)))
   (should (member (myrx assign)
                   '("\\(?:[[:alnum:]_]+\\)=\\(?:[[:alnum:]_]+\\)"
                     "[[:alnum:]_]+=[[:alnum:]_]+")))

   (should (member (myrx (* assign))
                  '("\\(?:\\(?:[[:alnum:]_]+\\)=\\(?:[[:alnum:]_]+\\)\\)*"
                    "\\(?:[[:alnum:]_]+=[[:alnum:]_]+\\)*")))))


(ert-deftest arx-form-function-returning-string-is-treated-as-regexp ()
  (with-myrx
   '((1: (:func (lambda (_name &optional arg)
                  (format "hello, %s." (or arg ""))))))
   (should (equal (myrx (1: "foo")) "hello, foo."))
   (should (equal (myrx (1:)) "hello, ."))))


(ert-deftest arx-form-function-returning-form ()
  (with-myrx
   '((repeat-unwind (:func (lambda (_form count elt)
                             `(seq ,@(make-list count elt))))))

   (should (equal (myrx (repeat-unwind 1 (or "foo" "bar")))
                  "\\(?:bar\\|foo\\)"))
   (should (equal (myrx (repeat-unwind 2 (or "foo" "bar")))
                  "\\(?:bar\\|foo\\)\\(?:bar\\|foo\\)"))))


(ert-deftest arx-form-function-fixed-number-of-args ()
  (with-myrx
   '((foobar (:func (lambda (_ foo bar) `(or ,foo ,bar)))))
   (should (equal (myrx (foobar "x" "y")) "[xy]"))

   (should-error-re (myrx-to-string '(foobar "x" "y" "z"))
                    "rx form [‘`]foobar['’] accepts at most 2 args")
   (should-error-re (myrx-to-string '(foobar "x"))
                    "rx form [‘`]foobar['’] requires at least 2 args")
   (should-error-re (myrx-to-string '(foobar))
                    "rx form [‘`]foobar['’] requires at least 2 args")
   ;; FIXME: fix this error message in Emacs27, which is confusing by default
   ;; (should-error-re (myrx-to-string 'foobar)
   ;;                  "rx [‘`]foobar['’] needs argument(s)")
   ))


(ert-deftest arx-form-function-max-args-overrides-rest-specification ()
  (with-myrx
   '((n: (:func
          (lambda (name index &rest args)
            (format "\\(?%d:%s\\)" index (apply 'concat args)))
          :max-args 3)))
   (should (equal (myrx (n: 1)) "\\(?1:\\)"))
   (should (equal (myrx (n: 1 "foo")) "\\(?1:foo\\)"))
   (should (equal (myrx (n: 1 "foo" "bar")) "\\(?1:foobar\\)"))

   ;; FIXME: fix this error message in Emacs27, which is confusing by default
   ;; (should-error-re (myrx-to-string 'n: 'nogroup)
   ;;                  "rx [‘`]n:['’] needs argument(s)")
   (should-error-re (myrx-to-string '(n:) 'nogroup)
                    "rx form [‘`]n:['’] requires at least 1 arg")
   (should-error-re (myrx-to-string '(n: 1 "foo" "bar" "baz"))
                    "rx form [‘`]n:['’] accepts at most 3 args")))


(ert-deftest arx-convenience-function-arx-and ()
  ; FIXME: implement arx-and in Emacs 27 (after implementing functions)
  ; TODO: add a test where arx-and inside a function uses a custom form
  :expected-result (if arx--new-rx :failed :passed)
  (should (equal (arx-and '()) ""))
  (should (equal (arx-and '("foo")) "foo"))
  (should (equal (arx-and '("bar" "foo")) "barfoo"))
  (should (equal (arx-and '("foo" (seq "bar" "baz"))) "foobarbaz"))
  (should (equal (arx-and '("foo" (or "bar" "baz"))) "foo\\(?:ba[rz]\\)")))


(ert-deftest arx-convenience-function-arx-or ()
  ; FIXME: implement arx-or in Emacs 27 (after implementing functions)
  ; TODO: add a test where arx-or inside a function uses a custom form
  :expected-result (if arx--new-rx :failed :passed)
  (should (equal (arx-or '()) ""))
  (should (equal (arx-or '("foo")) "foo"))
  (should (equal (arx-or '("foo" (seq "bar" "baz"))) "foo\\|barbaz"))
  ;; This over-grouping is an issue with `regexp-opt' that is used inside rx for
  ;; (or ...) forms consisting only of strings.
  (should (equal (arx-or '("bar" "foo")) "\\(?:bar\\|foo\\)"))
  (should (equal (arx-or '("foo" (or "bar" "baz"))) "foo\\|\\(?:ba[rz]\\)")))


(ert-deftest arx-check-func-form ()
  (should-error-re
   (define-arx--fn
     'myrx
     `((foo (:func ---foobarbaz-nonexistent---))))
   "Not a function: ---foobarbaz-nonexistent---"))


(ert-deftest arx-conditional-form-inclusion ()
  (with-myrx
   `((foo "foo")
     ,(when t '(bar "bar")))
   (should (equal (myrx-to-string 'foo t) "foo"))
   (should (equal (myrx-to-string 'bar t) "bar")))

  (with-myrx
   `((foo "foo")
     ,(when nil '(bar "hello")))

   (should (equal (myrx-to-string 'foo t) "foo"))
   (should-error-re
    (myrx-to-string 'bar t)
    "Unknown rx \\(form\\|symbol\\) [‘`]bar['’]")))


(defun arx--test-form (form foo bar))

(ert-deftest arx-eldoc-support ()
  ;; FIXME: resize window in batch mode?
  :expected-result (if noninteractive :failed :passed)
  (with-temp-buffer
    (emacs-lisp-mode)
    (eldoc-mode 1)
    (arx-minor-mode 1)
    (insert "\
 (myrx (foo s)
      (bar s)
      (baz s)
      (bazz s y)
      (qux s)
      (quux s)
      (yyy s)
      (zzz s))")
    (goto-char (point-min))
    (with-myrx
     `((foo "foo")
       (bar foo)
       (baz (:func arx--test-form))
       (bazz baz)
       (qux (:func (lambda (_ &rest args))))
       (quux qux)
       ;; (yyy (:func ---nzcvzxcvonexistentasdf---))
       (zzz xxxx))
     (should (re-search-forward "foo s"))
     (should-not (funcall eldoc-documentation-function))

     (should (re-search-forward "bar s"))
     (should-not (funcall eldoc-documentation-function))

     (should (re-search-forward "baz s"))
     (should (equal-including-properties
              (funcall eldoc-documentation-function)
              #("baz: (FOO BAR)" 0 3
                (face font-lock-function-name-face)
                6 9
                (face eldoc-highlight-function-argument))))

     (should (re-search-forward "bazz s y"))
     (should (equal-including-properties
              (funcall eldoc-documentation-function)
              #("bazz: (FOO BAR)" 0 4
                (face font-lock-function-name-face)
                11 14
                (face eldoc-highlight-function-argument))))

     (should (re-search-forward "qux s"))
     (should (equal-including-properties
              (funcall eldoc-documentation-function)
              #("qux: (&rest ARGS)" 0 3
                (face font-lock-function-name-face)
                12 16
                (face eldoc-highlight-function-argument))))

     (should (re-search-forward "quux s"))
     (should (equal-including-properties
              (funcall eldoc-documentation-function)
              #("quux: (&rest ARGS)" 0 4
                (face font-lock-function-name-face)
                13 17
                (face eldoc-highlight-function-argument))))

     (should (re-search-forward "yyy s"))
     (should-not (funcall eldoc-documentation-function))

     (should (re-search-forward "zzz s"))
     (should-not (funcall eldoc-documentation-function)))))


(if arx--new-rx
    (ert-deftest arx-rx-bindings-docstring ()
      (with-myrx
       '((name (regexp "[[:alnum:]_]+")))

       (should (equal (get 'myrx-bindings 'variable-documentation)
                      "\
List of bindings for `myrx' and `myrx-to-string' functions.

See `myrx' for a human readable list of defined forms.

See parameter BINDINGS for function `rx-let' for more information
about format of elements of this list."))))

  (ert-deftest arx-rx-constituents-docstring ()
    (with-myrx
     '((name (regexp "[[:alnum:]_]+")))

     (should (equal (get 'myrx-constituents 'variable-documentation)
                    "\
List of form definitions for `myrx' and `myrx-to-string' functions.

See `myrx' for a human readable list of defined forms.

See variable `rx-constituents' for more information about format
of elements of this list.")))))


(ert-deftest arx-rx-to-string-docstring ()
  (with-myrx
   '((name (regexp "[[:alnum:]_]+")))

   (should (equal (documentation 'myrx-to-string)
                  "\
Parse and produce code for regular expression FORM.

FORM is a regular expression in sexp form as supported by `myrx'.
NO-GROUP non-nil means don't put shy groups around the result."))))


(ert-deftest arx-rx-docstring ()
  (with-myrx
   '((name (regexp "[[:alnum:]_]+"))
     (foo-bar "foo.bar.baz"))

   (should (equal (documentation 'myrx)
                  "\
Translate regular expressions REGEXPS in sexp form to a regexp string.

See macro `rx' for more documentation on REGEXPS parameter.
This macro additionally supports the following forms:

`name'
    An alias for (regexp \"[[:alnum:]_]+\").

`foo-bar'
    A regexp matching literal string: \"foo.bar.baz\".

Use function `myrx-to-string' to do such a translation at run-time."))))
