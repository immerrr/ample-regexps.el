(require 'ample-regexps)
(require 're-builder)


(define-arx foobar-rx
  `((foo "foobarbaz")
    (bar foo)))
