;;;3.3.1
#!
(define print-with-call/cc
  (lambda ()
    (let ([n 0])
      (display (call/cc (lambda (k)
			  (k (+ n 1)...)))))))
!#
;;I really can't figure out how to solve this problem. Without assignments and recursions, plus the behaviour of returning to status in the past forgetting every modification, how is it possible that we can create an infinite loop printing 0 then 1 then 2 then 3? I checked the web and got this answer from scheme.com/tspl3/answer.html
(let ((k.n (call/cc (lambda (k) (cons k 0)))))
  (let ((k (car k.n)) (n (cdr k.n)))
    (write n)
    (newline)
    (k (cons k (+ n 1)))))
;;This is really smart! It constructs a list whose car is the continuation k and the cdr is the desired answer. After each round, list n gets a new element, like 0->(1 0)->(2 1 0) and the most part hard for me to think alike is understanding that when you send the continuation a value it just returns that value!

;;;3.3.2
(define (product lst)
  (let check ([flag #t])
    (if flag
	(let loop ([ls lst])
	  (if (null? ls) 1
	      (let ([lsa (car ls)])
		(if (zero? lsa) (check #f)
		    (* lsa (loop (cdr ls)))))))
	0)))
    
    
     
	   
