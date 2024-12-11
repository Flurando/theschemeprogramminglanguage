;;12.4.1
;;Just add this between (lambda (node word) and (cond...
(let ([word (string-downcase word)]) ... )

;;12.4.2
;;the modified version is as below
(define ger-words
  (λ (p)
    (let ([c (get-char p)])
      (if (eq? (char-type c) 'letter)
	  (list->string
	   (let loop ([c c])
	     (cons c
		   (if (or (memq (char-type (lookahead-char p)) '(letter digit)) (char=? c #\'))
		       (loop (get-char p))
		       '()))))
	  c))))
;;the only change is the or statement, introducing the check for whether c is the char '

;;12.4.3
;;define a common word list
(define common-words '("as" "is" "a" "the"))
;;wrap part of the procedure tree-print like this
(let ([word (tnode-word node)])
  (unless (member word common-words)
    (put-datum p (tnode-count node))
    (put-char p #\space)
    (put-string p word)
    (newline p)))
;;that's all

;;12.4.4
;;modified as below
(define get-word
  (λ (p)
    (let ([c (get-char p)])
      (if (eq? (char-type c) 'letter)
	  (let loop ([c c])
	    (string-append
	     (string c)
	     (if (memq (char-type (lookahead-char p))
		       '(letter digit))
		 (loop (get-char p))
		 "")))
	  c))))

;;12.4.5
;;the original is
(define tree
  (λ (node word)
    (cond
     [(null? node) (make-tnode word)]
     [(string=? word (tnode-word node))
      (begin
	(tnode-count-set! node
			  (1+ (tnode-count node)))
	node)]
     [(string<? word (tnode-word node))
      (begin
	(tnode-left-set! node
			 (tree (tnode-left node) word))
	node)]
     [else
      (begin
	(tnode-right-set! node
			  (tree (tnode-right node) word))
	node)])))

;;alongwith the representation of tree changed to 4 slots list
(define make-tnode
  (lambda (word count left right)
    (list word count left right)))
(define tnode-word
  (lambda (node)
    (car node)))
(define tnode-count
  (lambda (node)
    (cadr node)))
(define tnode-left
  (lambda (node)
    (caddr node)))
(define tnode-right
  (lambda (node)
    (cadddr node)))
(define tnode-count-set
  (lambda (node num)
    (make-tnode (tnode-word node)
		num
		(tnode-left node)
		(tnode-right node))))
(define tnode-left-set
  (lambda (node subnode)
    (make-tnode (tnode-word node)
		(tnode-count node)
		subnode
		(tnode-right node))))
(define tnode-right-set
  (lambda (node subnode)
    (make-tnode (tnode-word node)
		(tnode-count node)
		(tnode-left node)
		subnode)))
;;we can redesign it like
(define tree
  (λ (node word)
    (cond
     [(null? node) (make-tnode word 1 '() '())]
     [(string=? word (tnode-word node))
      (tnode-count-set node
		       (1+ (tnode-count node)))]
     [(string<? word (tnode-word node))
      (tnode-left-set node
		      (tree (tnode-left node) word))]
     [else
      (tnode-right-set node
		      (tree (tnode-right node) word))])))

;;12.4.6
(define dict!
  (lambda (dict word)
    (let ([count (hash-ref dict word)])
      (if count
	  (hash-set! dict word (1+ count))
	  (hash-set! dict word 1)))))

(use-modules (ice-9 format))
(define pretty-print-dict
  (lambda (dict)
    (hash-for-each-handle (lambda (handle)
			    (format #t "~a: ~d~%" (car handle) (cdr handle)))
			  dict)))
