(define-module (constraints)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  #:use-module (notifying-assignment)
  #:export-syntax (impose)
  #:export (constraint-variables untangle revoke-constraint!))

(use-modules (grand scheme)
	      (assignable-procedures)
	       (notifying-assignment))


(define (operator? x)
  (is x member '(+ - / *)))

(define (comparator? x)
  (is x member '(=)))

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* ((`(,<.> ,first ,second) formula)
		 ((operator? <.>)))
	(or (is factor used-in? first)
	    (is factor used-in? second)))))

(e.g.
 (is 'x used-in? '(* x 2)))

(e.g.
 (is '(f x) used-in? '(* 2 (f x))))

(define (extract-left operation formula)
  (match operation
    (`(+ ,left ,right)
     `(- ,formula ,right))
    
    (`(* ,left ,right)
     `(/ ,formula ,right))
    
    (`(- ,left ,right)
     `(+ ,formula ,right))
    
    (`(/ ,left ,right)
     `(* ,formula ,right))))

(define (extract-right operation formula)
  (match operation
    (`(+ ,left ,right)
     `(- ,formula ,left))
    
    (`(* ,left ,right)
     `(/ ,formula ,left))
    
    (`(- ,left ,right)
     `(- ,left ,formula))
    
    (`(/ ,left ,right)
     `(/ ,left ,formula))))

(define (extract factor #;from source #;into target)
  (if (equal? factor source)
      target
      (let ((`(,<.> ,first ,second) source))
	(cond ((or (isnt <.> operator?)
		   (and (is factor used-in? first)
			(is factor used-in? second)))
	       'oops)
	      ((is factor used-in? first)
	       (let ((target* (extract-left source target)))
		 (extract factor first target*)))
	      ((is factor used-in? second)
	       (let ((target* (extract-right source target)))
		 (extract factor second target*)))))))

(define (untangle factor #;from equation)
  (let ((`(,>?< ,left ,right) equation))
    (cond ((and (is factor used-in? left)
		(is factor used-in? right))
	   `(oops: ,equation))
	  ((is factor used-in? left)
	   (extract factor #;from left #;into right))
	  ((is factor used-in? right)
	   (extract factor #;from right #;into left)))))

(e.g.
 (untangle 'a #;from '(= (/ a b) (/ c d))) ===> (* (/ c d) b))

(define (factors formula)
  (or (and-let* ((`(,<.> . ,operands) formula)
		 ((or (operator? <.>) (comparator? <.>))))
	(apply union (map factors operands)))
      `(,formula)))

(e.g.
 (is (factors '(= (/ a b) (/ c d)))
     same-set?
     '(a b c d)))

(e.g.
 (is (factors '(= (* (f x) (g y)) (z w)))
     same-set?
     '((f x) (g y) (z w))))

(define (constraint-variables formula)
  (filter (isnt _ number?) (factors formula)))

(define-macro (impose constraint)
  (let ((expressions (constraint-variables constraint))
	(_ (make-symbol "_")))
    `(list . ,(map (lambda (expression)
		      (let ((others (delete expression
					    #;from expressions)))
			`(on-change ,expression
				    (primitive-lambda ,_
				      . ,(map (lambda (observer)
						`(set! ,observer
						       ,(untangle observer
								  constraint)))
					      others)))))
		    expressions))))

(define (revoke-constraint! constraint)
  (for notification in constraint
    (revoke-notification! notification)))

#|
;; We don't want these symbols to clutter global namespace

(e.g.
 (begin
   (define x 5)
   (define y 1/5)
   (impose (= x (/ 1 y)))
   (set! x 6)
   y) ===> 1/6)
 
(e.g.
 (begin
   (define a 2)
   (define b 3)
   (define c 6)
   (impose (= (* a b) c))
   (print (observers a))
   (set! a 5)
   (values a b c))
 ===> 5 3 15)
|#
