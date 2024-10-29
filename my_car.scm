(define list (lambda x x))

(define (compose proc1 proc2)
  (lambda (x) (proc1 (proc2 (x)))))

(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define caar (compose car car))
(define cddr (compose cdr cdr))
(define caaar (compose car (compose car car)))

(define length
  (lambda (x)
    (if (null? x)
	0
	(+ 1 (length (cdr x))))))

(define make-counter
  (lambda ()
    (let ([n 0])
      (lambda ()
	(set! n (+ n 1))
	n))))

(define (make-list n obj)
  (if (= n 0) '()
      (cons obj (make-list (- n 1) obj))))

(define (list-ref somelist index)
  (if (= index 0)
      (car somelist)
      (list-ref (cdr somelist) (- index 1))))

(define (list-tail somelist index)
  (if (= index 0)
      somelist
      (list-tail (cdr somelist) (- index 1))))

(define (shorter? a b)
  (if (not (and (list? a) (list? b)))
      (error "wrong type argument! Expecting two lists"))
  (cond [(and (null? a) (not (null? b))) 'a]
	[(and (not (null? a)) (null? b)) 'b]
	[(and (null? a) (null? b)) 'a]
	[else (shorter? (cdr a) (cdr b))]))

(define (shorter a b)
  (if (eq? (shorter? a b) 'b)
      b
      a))

(define (even? n)
  (if (= n 0) #t
      (odd? (- n 1))))
(define (odd? n)
  (if (= n 0) #f
      (even? (- n 1))))

(define (reverse lst);error! return nothing and is using assignment instead of recursion
  (cond [(null? lst) '()])
  (let ([receiver '()]
	[helper '()]
	[helperproc reverse])
    (begin
      (set! receiver (fetch-last lst))
      (set! helper (copy-until-last-2 lst))
      (if (not (null? (cdr lst)))
	  (helperproc (cdr lst))))))

(define (fetch-last lst);error! only return '()
  (cond [(null? lst) '()]
	[(null? (cdr lst)) '()]
	[else (fetch-last (cdr lst))]))

(define (copy-until-last-2 lst)
  (cond [(null? lst) '()]
	[(null? (cdr lst)) '()]
	[else (cons (car lst) (copy-until-last-2 (cdr lst)))]))

(define (transpose lst)
  (let ([lst1 '()]
	[lst2 '()])
    (map (lambda (p)
	   (set! lst1 (cons (car p) lst1))
	   (set! lst2 (cons (cdr p) lst2))) lst)
    (cons (reverse lst1) (reverse lst2))))
