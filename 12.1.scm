(use-modules (ice-9 format))

(define vector-all-number?
  (lambda (v)
    (let ([len (vector-length v)])
      (let loop ([index 0])
	(if (>= index len) #t
	    (if (number? (vector-ref v index))
		(loop (1+ index))
		#f))))))

(define make-matrix
  (lambda (row column)
    ((do ([m (make-vector rows)]
          [i 0 (+ i 1)])
         ((= i rows) m)
       (vector-set! m i (make-vector columns))))))

(define matrix?
  (lambda (x)
    (and (vector? x)
         (> (vector-length x) 0)
	 (vector? (vector-ref x 0))
	 (let ([num (vector-length (vector-ref x 0))])
	   (let loop ([index 0])
	     (if (>= index (vector-length x)) #t
		 (if (and (vector? (vector-ref x index)) (= num (vector-ref x index)) (vector-all-number? (vector-ref x index)))
		     (loop (1+ index))
		     #f))))
	 

(define matrix-rows
  (lambda (x)
    (vector-length x)))

(define matrix-ref
  (lambda (m i j)
    (vector-ref (vector-ref m i) j)))

(define matrix-set!
  (lambda (m i j x)
    (vector-set! (vector-ref m i) j x)))

(define mat-sca-mul
  (lambda (m x)
    (let* ([nr (matrix-rows m)]
           [nc (matrix-columns m)]
           [r (make-matrix nr nc)])
      (do ([i 0 (+ i 1)])
          ((= i nr) r)
        (do ([j 0 (+ j 1)])
            ((= j nc))
          (matrix-set! r i j (* x (matrix-ref m i j))))))))

(define mat-mat-mul
  (lambda (m1 m2)
    (let* ([nr1 (matrix-rows m1)]
           [nr2 (matrix-rows m2)]
           [nc2 (matrix-columns m2)]
           [r (make-matrix nr1 nc2)])
      (unless (= (matrix-columns m1) nr2) (match-error m1 m2))
      (do ([i 0 (+ i 1)])
          ((= i nr1) r)
        (do ([j 0 (+ j 1)])
            ((= j nc2))
          (do ([k 0 (+ k 1)]
               [a 0 (+ a
                       (* (matrix-ref m1 i k)
                          (matrix-ref m2 k j)))])
              ((= k nr2)
               (matrix-set! r i j a))))))))

(define type-error
  (lambda (what)
    (assertion-violation 'mul
			 "not a number or matrix"
			 what)))

(define match-error
  (lambda (what1 what2)
    (error  (format #f "incompatible operands\n~a ~a"
		    what1
		    what2))))
(define mul
  (lambda (x y)
    (cond
     [(number? x)
      (cond
       [(matrix? y) (mat-sca-mul y x)]
       [else (type-error y)])]
     [(matrix? x)
      (cond
       [(number? y) (mat-sca-mul x y)]
       [(matrix? y) (mat-mat-mul x y)]
       [else (type-error y)])]
     [else (type-error x)])))

;;;12.1.1
;;No idea:( I am wondering how could it be possible to overlap the original * while still using it in scalar multiply with scalar situation -- Obviously discarding the original * is not a valid option and replacing * with recursive adds is not okey for me too.

;;;12.1.2
;;somehow nasty but I think it works...though untested

;;;12.1.3
;;
