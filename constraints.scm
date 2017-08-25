(use-modules (grand scheme))


(define (operator? x)
  ...)

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* (((<.> first second) formula)
		 ((operator? <.>)))
	(or (is factor used-in? first)
	    (is factor used-in? second)))))


(define (inverse ...)
  ...)

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
