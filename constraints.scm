(use-modules (grand scheme))

(define (operator? x)
  ...)

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* (((<.> first second) formula)
		 ((operator? <.>)))
	(or (is factor used-in? first)
	    (is factor used-in? second)))))

(define (untangle factor #;from formula)
  ...)

(e.g.
 (untangle 'a #;from '(= (/ a b) (/ c d))) ===> (* b (/ c d))
 (untangle 'b #;from '(= (/ a b) (/ c d))) ===> ...)
