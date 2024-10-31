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
