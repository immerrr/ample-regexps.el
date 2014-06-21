(require 'ample-regexps)

(defmacro with-myrx (arx-forms &rest body)
  `(progn
     (define-arx myrx ,@arx-forms)
     (unwind-protect
         (progn ,@body)
       (fmakunbound 'myrx-to-string)
       (fmakunbound 'myrx))))


(provide 'test-helper)
