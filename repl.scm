(define env '())
(define extend-env!
  (lambda (key value)
    (cons (cons key value) env)))
