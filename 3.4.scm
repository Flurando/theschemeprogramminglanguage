;;;3.4.1
(define reciprocal
  (lambda (n success failure)
    (if (zero? n)
	(failure n)
	(success (/ 1 n)))))

;;;3.4.2
(define factorial
  (lambda (x retry)
    (if (zero? x)
	((car retry) (cdr retry))
	(* x (factorial (- x 1) (car retry) (cdr retry))))))

;;;3.4.3
(define reciprocals
  (lambda (ls succeed failure)
    (map (lambda (x)
	   (if (zero? x)
	       (failure "zero found")
	       (success (/ 1 x)))
	   ls))))
