;;;2.9.1
(define make-counter
  (lambda (a b)
    (let ([init-value a]
	  [step b])
      (lambda ()
	(begin
	  (set! init-value (+ init-value step))
	  init-value)))))

;;;redefine the queue type
(define my-make-queue
  (let ([p '()])
    (lambda (opt . args)
      (cond [(eq? opt 'putq!)
	     (begin
	       (set! p (append p (car args))))]
	    [(eq? opt 'getq)
	     (begin
	       (car p))]
	    [(eq? opt 'delq!)
	     (begin
	       (set! p (cdr p)))]))))
