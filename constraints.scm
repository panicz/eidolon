(use-modules (grand scheme))


(define (operator? x)
  ...)

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* (((<.> first second) formula)
		 ((operator? <.>)))
	(or (is factor used-in? first)
	    (is factor used-in? second)))))


(define (extract-left operation formula)
  (match operation
    (`(+ ,left ,right)
     `(- ,formula ,right))
    (`(* ,left ,right)
     `(/ ,formula right))
    (`(- ,left ,right)
     `(+ ,formula ,right))
    (`(/ ,left ,right)
     `(* ,fomula ,right))))


(assert (lambda (a b)
	  (= (* a b) (* b a))))

(assert (lambda (a b)
	  (= (+ a b) (+ b a))))

(assert (lambda (a b c)
	  (equal? (= (+ a b) c) (= a (- c b)))))

(assert (lambda (a b c)
	  (equal? (= (+ a b) c) (= b (- c a)))))

(assert (lambda (a b c)
	  (if (isnt b zero?)
	      (equal? (= (* a b) c) (= a (/ c b))))))

(assert (lambda (a b c)
	  (if (isnt a zero?)
	      (equal? (= (* a b) c) (= b (/ c a))))))

(assert (lambda (a b c)
	  (if (isnt b zero?)
	      (equal? (= (/ a b) c) (= a (* c b))))))

(assert (lambda (a b c)
	  (if (and (isnt b zero?) (isnt c zero?))
	      (equal? (= (/ a b) c) (= 


(define (extract-right operation formula)
  (match operation
    (`(+ ,left ,right)
     `(- ,formula ,left))
    (`(* ,left ,right)
     `(/ ,formula ,left))
    (`(
    
    
(e.g.
 (strip-left '(+ a b) 'c) ===> (- c b))



(define (extract factor #;from source #;into target)
  (if (equal? factor source)
      target
      (let (((<.> first second) source))
	(cond ((and (is factor used-in? first)
		    (is factor used-in? second))
	       'oops)
	      ((is factor used-in? first)
	       
	       ...)
	      ((is factor used-in? right)
	       ...)))))

(define (untangle factor #;from equation)
  (let ((('= left right) equation))
    (cond ((and (is factor used-in? left)
		(is factor used-in? right))
	   'oops)
	  ((is factor used-in? left)
	   (extract factor #;from left #;into right))
	  ((is factor used-in? right)
	   (extract factor #;from right #;into left)))))


(e.g.
 (untangle 'a #;from '(= (/ a b) (/ c d))) ===> (* b (/ c d))
 (untangle 'b #;from '(= (/ a b) (/ c d))) ===> )
