;;;12.2.1
;;Because domerge requires two lists even if there is only one element in the list and the coder doesn't want to put the work there. Just a single ref on stack and a list on heap saved, in the price of an additional check.

;;;12.2.2
;;The memory needed to store that and the effort of making a new holder every sorting or merging step

;;;12.2.3
;;
(define sort!
  (lambda* (pred lst #:optional (i1 0) (i2 (1- (length lst))))
    (if (= (1+ i1) i2)
	(if (pred (list-ref lst i1) (list-ref lst i2))
	    (let ([tmp (list-ref lst i2)])
	      (list-set! lst i2 (list-ref lst i1))
	      (list-set! lst i1 tmp)))
	(let ([i-middle (+ i1 (quotient (- i2 i1) 2))])
	  (when (< i1 i-middle) (sort! pred lst i1 i-middle))
	  (when (< (1+ i-middle) i2) (sort! pred lst (1+ i-middle) i2))
	  (merge! pred lst i1 i-middle i2)))))
			       

(define merge!
  (lambda (pred lst i1 i2 i3)
    (cond [(= i2 i3) #f]
	  [else (if (pred (list-ref lst i1) (list-ref lst (1+ i2)))
		    (begin (let ([tmp (list-ref lst (1+ i2))])
			     (let loop ([index (1+ i2)])
			       (let ([index-minus-1 (1- index)])
				 (list-set! lst index (list-ref lst index-minus-1))
				 (unless (= index-minus-1 i1) (loop index-minus-1))))
			     (list-set! lst i1 tmp))
			   (merge! pred lst (1+ i1) (1+ i2) i3))
		    (unless (= i1 i2)
		      (merge! pred lst (1+ i1) i2 i3)))])))

(use-modules (srfi srfi-64))
(test-begin "sort test")
(let ([a (list 2 1)])
  (sort! > a)
  (test-assert (equal? a '(1 2))))
(test-end "sort test")
(test-begin "merge test")
(let ([a (list 1 2 3 4 1 5 7)])
  (merge! > a 0 3 6)
  (test-assert (equal? a '(1 1 2 3 4 5 7))))
(test-end "merge test")
(test-begin "merge-sort test")
(let ([a (list 3 2 1)])
  (sort! > a)
  (test-assert (equal? a '(1 2 3))))
(let ([a (list 4 2 121 1 214 41 44 2 1)])
  (sort! > a)
  (test-assert (equal? a '(1 1 2 2 4 41 44 121 214))))
(test-end "merge-sort test")
