;;;3.1.1
#!
((lambda (x) (and x (memv 'b x))) (memv 'a ls))
((lambda (x) (if x (and (memv 'b x)) #f)) (memv 'a ls))
((lambda (x) (if x (memv 'b x) #f)) (memv 'a ls))
!#

;;;3.1.2
#!
(let ([t (memv x '(a b c))])
  (if t t (or (list x))))
((lambda (t) (if t t (or (list x)))) (memv x '(a b c)))
((lambda (t) (if t t (list x))) (memv x '(a b c)))
!#

;;;3.1.3
(define-syntax let*
  (syntax-rules ()
    [(_ ((x v)) e ...) (let ((x v)) e ...)]
    [(_ ((x1 v1) (x2 v2) ...) e ...)
     (let ((x1 v1)) (let* ((x2 v2) ...) e ...))]))

;;;3.1.4
(define-syntax when
  (syntax-rules ()
    [(_ test expr ...)
     (if test
	 (begin expr ...))]))

(define-syntax unless
  (syntax-rules ()
    [(_ test expr ...)
     (when (not test) expr ...)]))
