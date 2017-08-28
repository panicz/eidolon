(use-modules (grand scheme))


(define (operator? x)
  ...)

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* (((<.> first second) formula)
		 #;((operator? <.>)))
	(or (is factor used-in? first)
	    (is factor used-in? second)))))


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
      (let (((<.> first second) source))
	(cond ((and (is factor used-in? first)
		    (is factor used-in? second))
	       'oops)
	      ((is factor used-in? first)
	       (let ((target* (extract-left source target)))
		 (extract factor first target*)))
	      ((is factor used-in? second)
	       (let ((target* (extract-right source target)))
		 (extract factor second target*)))))))

(define (untangle factor #;from equation)
  (let ((('= left right) equation))
    (cond ((and (is factor used-in? left)
		(is factor used-in? right))
	   'oops)
	  ((is factor used-in? left)
	   (extract factor #;from left #;into right))
	  ((is factor used-in? right)
	   (extract factor #;from right #;into left)))))

;;(untangle 'e '(= (* a (/ b (+ c (- d e)))) f))

(e.g.
 (untangle 'a #;from '(= (/ a b) (/ c d))) ===> (* (/ c d) b))



(define (factor? x)
  (or (number? x)
      (symbol? x)))

(define (factors formula)
  (if (factor? formula)
      `(,formula)
      (let (((operator . operands) formula))
	(apply union (map factors operands)))))
(e.g.
 (factors '(= (/ a b) (/ c d))) ===> (c d b a))

(define (assert x)
  (assert (and (equation? x)
	       (every (lambda (factor)
			(or (location? factor)
			    (number? factor)))
		      (factors x))))
  (let ((locations (filter location? (factors x))))
    ...))
    
    
