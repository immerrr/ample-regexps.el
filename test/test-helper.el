(require 'ample-regexps)

(defmacro with-myrx (arx-forms &rest body)
  (declare (debug (&rest form)))
  `(progn
     (define-arx myrx ,arx-forms)
     (unwind-protect
         (progn ,@body)
       (fmakunbound 'myrx-to-string)
       (fmakunbound 'myrx))))

(defmacro should-error-re (expr pattern)
  `(should (string-match ,pattern (cadr (should-error ,expr)))))


(provide 'test-helper)
