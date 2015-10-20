;; do nothing

(load-file "ample-regexps.el")
(require 're-builder)


(define-arx foobar
  `((foo "foobarbaz")
    (bar foo)))
