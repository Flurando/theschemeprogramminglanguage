;;;3.2.1
;;jumped because I don't quite understand it

;;;3.2.2
(define factor
  (lambda (n)
    (letrec ([f (lambda (n i)
		  (cond
		   [(>= i n) (list n)]
		   [(integer? (/ n i))
		    (cons i (f (/ n i) i))]
		   [else (f n (+ i 1))]))])
      (f n 2))))

;;;3.2.3
;;Impossible because named let can't take two lambda at the same time

;;;3.2.4
(define count-1 0)
(define fibonacci-1
  (lambda (n)
    (let fib ([i n])
      (cond
       [(= i 0) 0]
       [(= i 1) 1]
       [else (+ (begin (set! count-1 (+ count-1 1)) (fib (- i 1)))
		(begin (set! count-1 (+ count-1 1)) (fib (- i 2))))]))))
(define count-2 0)
(define fibonacci-2
  (lambda (n)
    (if (= n 0)
	0
	(let fib ([i n] [a1 1] [a2 0])
	  (if (= i 1)
	      a1
	      (begin (set! count-2 (+ count-2 1))
		     (fib (- i 1) (+ a1 a2) a1)))))))
;;I tried both procedure inputing 10, they output the same value 55 with count-1 being 176 and count-2 being 9

;;;3.2.5
(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]
    [(_ name ((x e) ...) b1 b2 ...)
     (letrec ((name (lambda (x ...) b1 b2 ...))) (name e ...))]))

;;;3.2.6
;;I think the error is that if used in that way (even! and odd!), since this version didn't pull (_ e) e out seperately, leaving everything to the last while allowing one (_ e) to be expanded into (let ((t e)) (if t t (_))), making even? no longer a tail call thus may have a risk of stack overflow (Just my guess, can't think of any other possibilities myself, really!)

;;;3.2.7
;;The most important problem to solve is that I have to make sure 2 is handled rightly and all the recursive calls are tail calls
(define factor
  (lambda (n)
    (when (= n 1) 1)
    (when (= n 2) 2)
    (when (= n 3) 3)
    (if (odd? n) ((let f ([n n] [i 3])
		    (when (> i (sqrt n)) (list n))
		    (let ([u (/ n i)])
		      (if (integer? u) (cons i (f u i))
			  (f n (+ i 2))))))
	(let ([lst (let f ([n1 n])
		     (let ([u (/ n1 2)])
		       (set! n u)
		       (if (even? u) (cons 2 (f u))
			   (list 2))))])
	  (let g ([n n] [i 3])
	    (when (> i (sqrt n)) (cons n lst))
	    (let ([u (/ n i)])
	      (if (integer? u) (cons i (g u i))
		  (g n (+ i 2)))))))))
