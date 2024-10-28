(define list (lambda x x))

(define car (lambda (x . y) x))
(define cdr (lambda (x . y) y))

(define (compose proc1 proc2)
  (lambda (x) (proc1 (proc2 (x)))))

(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define caar (compose car car))
(define cddr (compose cdr cdr))
(define caaar (compose car (compose car car)))
