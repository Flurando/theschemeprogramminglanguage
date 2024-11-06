;;;2.9.1
(define make-counter
  (lambda (a b)
    (let ([init-value a]
	  [step b])
      (lambda ()
	  (set! init-value (+ init-value step))
	  init-value))))

;;;2.9.2 & 2.9.3
(define (make-stack)
  (let ([lst '()])
    (lambda (msg . args)
      (case msg
	[(empty) (null? lst)]
	[(push!) (cons (car args) lst)]
	[(top) (car lst)]
	[(pop) (set! lst (cdr lst))]
	[(ref) (let loop ([l lst]
			   [c (car args)])
		  (if (= c 0) (car lst)
		      (loop (cdr lst) (- c 1))))]
	[(set!) (let loop ([l lst]
			   [c (car args)])
		  (if (= c 0) (set-car! lst (cadr args))
		      (loop (cdr lst) (- c 1))))]
	[else "oops"]))))

;;;redefine the queue type & 2.9.5 (&2.9.6)
(define my-make-queue
  (let ([p '()])
    (lambda (opt . args)
      (cond [(eq? opt 'putq!)
	     (begin
	       (set! p (append p (car args))))]
	    [(eq? opt 'getq)
	     (begin
	       (if (null? p) (raise-exception (make-exception-with-message "wrong")))
	       (car p))]
	    [(eq? opt 'delq!)
	     (begin
	       (if (null? p) (raise-exception (make-exception-with-message "wrong")))
	       (set! p (cdr p)))]
	    [(eq? opt 'emptyq?)
	     (begin
	       (null? p))]))))

;;;2.9.4
(define (make-stack n)
  (let ([lst (make-vector n)])
    (lambda (msg . args)
      (case msg
	[(empty) (null? lst)]
	[(push!) (cons (car args) lst)]
	[(top) (car lst)]
	[(pop) (set! lst (cdr lst))]
	[(ref) (let ([index (car args)])
		  (vector-ref lst index))]
	[(set!) (let ([index (car args)]
		       [v (cadr args)])
		   (vector-set! lst index v))]
	[else "oops"]))))

;;;2.9.7
;;it shows (a . #0#)
;;never return
;;output an error

;;;2.9.8 (Just copyed from the book, not mine thought -- It is really hard to understand!)
(define (list? a)
  (let race ([h a] [t a])
    (if (pair? h)
	(let ([h (cdr h)])
	  (if (and (pair? h) (not (eq? h t)))
	      (race (cdr h) (cdr t))
	      (null? h)))
	(null? h))))
