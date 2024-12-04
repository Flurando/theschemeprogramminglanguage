;;;below is the original code from TSPL
;;;All rights reserved for them
;;;copied here just for convinience
(library (tspl sets)
  (export set-of set-cons in is)
  (import (rnrs))

 ; set-of uses helper syntactic extension set-of-help, passing it
 ; an initial base expression of '()
  (define-syntax set-of
    (syntax-rules ()
      [(_ e m ...)
       (set-of-help e '() m ...)]))

 ; set-of-help recognizes in, is, and predicate expressions and
 ; changes them into nested named let, let, and if expressions.
  (define-syntax set-of-help
    (syntax-rules (in is)
      [(_ e base) (set-cons e base)]
      [(_ e base (x in s) m ...)
       (let loop ([set s])
         (if (null? set)
             base
             (let ([x (car set)])
               (set-of-help e (loop (cdr set)) m ...))))]
      [(_ e base (x is y) m ...)
       (let ([x y]) (set-of-help e base m ...))]
      [(_ e base p m ...)
       (if p (set-of-help e base m ...) base)]))

 ; since in and is are used as auxiliary keywords by set-of, the
 ; library must export definitions for them as well
  (define-syntax in
    (lambda (x)
      (syntax-violation 'in "misplaced auxiliary keyword" x)))

  (define-syntax is
    (lambda (x)
      (syntax-violation 'is "misplaced auxiliary keyword" x)))

 ; set-cons returns the original set y if x is already in y.
  (define set-cons
    (lambda (x y)
      (if (memv x y)
          y
          (cons x y)))))
;;;above ends the original copied code.
         
;;12.3.1
(define union
  (lambda x
    (set-of z (y in x) (z in y))))

;;12.3.2
;;the original example(wrong) is
(define map1
  (lambda (f ls)
    (set-of (f x) (x in ls))))
(map1 - '(1 2 3 2))
;;This does not work because set-of only stores the different elements in using set-cons, if we want to disable this we just need to use cons instead of set-cons

;;12.3.3
(define set-cons
  (lambda (x y)
    (let ([y-length (length y)])
      (let loop ([index 0])
	(let ([check (list-ref y index)])
	  (if (= x check)
	      (break y)
	      (if (< x check)
		  (if (zero? index)
		      (cons x y)
		      (append (list-head y index) (list x) (list-tail y index)))
		  (if (>= index (1- y-length))
		      (append y (list x))
		      (loop (1+ index))))))))))
		      
