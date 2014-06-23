;; -*-no-byte-compile: t; -*-
(require 'ert)
(require 'test-helper
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "test-helper.el"))

(ert-deftest arx-empty-list ()
  (with-myrx
   '()
   (should (equal (myrx "foobar") "foobar"))))

(ert-deftest arx-alias-for-literal-basic ()
  (with-myrx
   '((hello "Hello"))
   (should (equal (myrx hello) "Hello"))
   (should (equal (myrx hello ", world") "Hello, world"))
   (should (equal (myrx (or hello ", world")) "Hello\\|, world"))
   (should (equal (myrx (* hello)) "\\(?:Hello\\)*"))))


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
   (should (equal (myrx assign) "[[:alnum:]_]+=[[:alnum:]_]+"))))


(ert-deftest arx-form-function-returning-regexp ()
  (with-myrx
   '((1: (:func (lambda (name &rest args)
                  (concat "\\(?1:" (arx-and args) "\\)")))))
   (should (equal (myrx (1: "foo"))
                  "\\(?1:foo\\)"))
   (should (equal (myrx (1: "foo" "bar" "baz"))
                  "\\(?1:foobarbaz\\)"))

   (should (equal (myrx (1:))
                  "\\(?1:\\)"))
   ;; should-error doesn't work with macros, so fall back to `myrx-to-string'
   (should (equal (myrx-to-string '(1:) 'nogroup)
                  "\\(?1:\\)"))
   (should-error (myrx-to-string '1: 'nogroup))))


(ert-deftest arx-form-function-returning-form ()
  (with-myrx
   '((sym (:func
           (lambda (name &rest args)
             (if (> (length args) 1)
                 (setq args `(or ,@args)))
             `(seq symbol-start ,@args symbol-stop))))
     (should (equal (myrx (sym "foo"))
                    "\\_<foo\\_>"))
     (should (equal (myrx (sym "foo" "bar"))
                    "\\_<\\(?:foo\\|bar\\)\\_>"))
     (should (equal (myrx (sym (seq (or "foo" "bar") "baz")))
                    "\\_<\\(?:\\(?:foo\\|bar\\)baz\\)\\_>"))

     ;; This test hardly makes any sense regex-wise, but empty arguments are not
     ;; forbidden in the form definition, so should work.
     (should (equal (myrx (sym)) "\\_<\\_>")))))



(ert-deftest arx-form-function-min-args ()
  (with-myrx
   '((n: (:func
          (lambda (name index &rest args)
            (concat (format "\\(?%d:" index) (arx-and args) "\\)"))
          :min-args 1)))
   (should (equal (myrx (n: 5 "foo"))
                  "\\(?5:foo\\)"))
   (should (equal (myrx (n: 1 "foo" "bar" "baz"))
                  "\\(?1:foobarbaz\\)"))
   (should-error (myrx-to-string 'n: 'nogroup))
   (should-error (myrx-to-string '(n:) 'nogroup))))



(ert-deftest arx-convenience-function-arx-and ()
  (should (equal (arx-and '()) ""))
  (should (equal (arx-and '("foo")) "foo"))
  (should (equal (arx-and '("bar" "foo")) "barfoo"))
  (should (equal (arx-and '("foo" (seq "bar" "baz"))) "foobarbaz"))
  (should (equal (arx-and '("foo" (or "bar" "baz"))) "foo\\(?:ba[rz]\\)")))


(ert-deftest arx-convenience-function-arx-or ()
 (should (equal (arx-or '()) ""))
 (should (equal (arx-or '("foo")) "foo"))
 (should (equal (arx-or '("foo" (seq "bar" "baz"))) "foo\\|barbaz"))
 ;; This over-grouping is an issue with `regexp-opt' that is used inside rx for
 ;; (or ...) forms consisting only of strings.
 (should (equal (arx-or '("bar" "foo")) "\\(?:bar\\|foo\\)"))
 (should (equal (arx-or '("foo" (or "bar" "baz"))) "foo\\|\\(?:ba[rz]\\)")))


(ert-deftest arx-conditional-form-inclusion ()
  (with-myrx
   `((foo "foo")
     ,(when t '(bar "hello")))
   (should (assq 'foo myrx-constituents))
   (should (assq 'bar myrx-constituents)))

  (with-myrx
   `((foo "foo")
     ,(when nil '(bar "hello")))
   (should (assq 'foo myrx-constituents))
   (should-not (assq 'bar myrx-constituents))
   (should-not (assq nil myrx-constituents))))
