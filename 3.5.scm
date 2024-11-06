;;;This time, I'll try to rewrite calc entirely to meet all the answers' requirements.
(use-modules (ice-9 exceptions))
(define (calc expr)
  (let ([complain
    (lambda (msg)
      (raise-exception (make-exception-with-message msg)))])
    (if (number? expr)
	expr
	(if (null? (cdr expr))
	    (calc (car expr))
	    (let ([car-expr (car expr)])
	      (let ([cdr-expr (cdr expr)])
		(case car-expr
		  [(add) (+ (calc (car cdr-expr)) (calc (cdr cdr-expr)))]
		  [(div) (/ (calc (car cdr-expr)) (calc (cdr cdr-expr)))]
		  [(minus) (- 0 (calc cdr-expr))]
		  [(mul) (* (calc (car cdr-expr)) (calc (cdr cdr-expr)))]
		  [else (complain "invalid argument: ")])))))))
