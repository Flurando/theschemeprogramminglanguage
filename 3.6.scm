;;;This time, we are required to define a module to calc gpa, which should be able to handle "x", count distribution of each grade, and histogram function to write "visualized" data to any output-port
(define-module (gpa gpa)
  #:export '(gpa distribution histogram))

(define (letter->number l)
  (case l
    [(a) 4.0]
    [(b) 3.0]
    [(c) 2.0]
    [(d) 1.0]
    [(f) 0.0]
    [(x) 'out]
    [else (raise-exception (make-exception-with-message "wrong argument! please make sure you only use a, b, c, d, f, x"))]))

(define (letter->number* lst)
  (if (null? lst) '()
      (let ([car-list (car lst)])
	(if (eq? 'x car-list)
	    (letter->number* (cdr lst))
	    (cons (letter->number car-list) (letter->number* (cdr lst)))))))

(define (between? x i j)
  (if (or (and (< x i) (< x j))
	  (and (>= x i) (>= x j)))
      #f
      #t))

(define (gpa->grade x)
  (cond
   [(between? x 0.0 0.5) 'f]
   [(between? x 0.5 1.5) 'd]
   [(between? x 1.5 2.5) 'c]
   [(between? x 2.5 3.5) 'b]
   [else 'a]))

(define sum-up
  (lambda (lst)
    (cond
     [(null? lst) 0]
     [(number? lst) lst]
     [else (+ (sum-up (car lst))
	      (sum-up (cdr lst)))])))

(define length
  (lambda (lst)
    (cond
     [(null? lst) 0]
     [(number? lst) 1]
     [else (+ 1 (length (cdr lst)))])))

(define-syntax gpa
  (syntax-rules ()
    [(_ g1 g2 ...)
     (let ([lst (letter->number* '(g1 g2 ...))])
       (/ (sum-up ls) (length ls)))]))

(define (grade-counter lst holder)
  (let loop ([ls lst])
    (cond
     [(null? ls) '()]
     [(mem (car ls) '(a b c d f))
      (begin
	(assoc-set! holder (car ls) (+ 1 (assoc-ref (car ls))))
	(loop (cdr lst)))]))
  holder)

(define alist->needed
  (lambda (lst)
    (cond
     [(null? lst) '()]
     [(pair? lst) (let ([p lst])
		    (list (cdr p) (car p)))]
     [else (cons (alist->needed (car lst)) (alist->needed (cdr lst)))])))

(define-syntax distribution
  (syntax-rules ()
    [(_ g1 g2 ...)
     (alist->needed (grade-counter '(g1 g2 ...) '((a . 0) (b . 0) (c . 0) (d . 0) (f . 0))))]))

(define (number->* num)
  (let loop ([n num])
    (if (= n 0) ""
	(string-append "*" (loop (- num 1))))))

;;explicit for use in histogram
(define-syntax printer
  (syntax-rules ()
    [(_ ?) (begin
	     (display ? port)
	     (display ": " port)
	     (display (number->* (assoc-ref alst ?)) port)
	     (newline))]))

(define histogram
  (lambda (port alst)
    (printer 'a)
    (printer 'b)
    (printer 'c)
    (printer 'd)
    (printer 'f)))
    
