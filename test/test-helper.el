;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-

(require 'ample-regexps)

(defmacro with-myrx (arx-forms &rest body)
  (declare (debug (&rest form)))
  `(progn
     (define-arx myrx ,arx-forms)
     (unwind-protect
         (progn ,@body)
       (makunbound 'myrx-constituents)
       (fmakunbound 'myrx-to-string)
       (fmakunbound 'myrx))))

(defmacro should-error-re (expr pattern)
  "Macro wrapper to test that EXPR fails with error matching PATTERN."
  `(progn
     (let* ((result (should-error ,expr))
             (msg (cadr result)))
       (should (string-match ,pattern msg)))))


(provide 'test-helper)
