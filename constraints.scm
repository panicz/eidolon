(use-modules (grand scheme))

(define (operator? x)
  (is x member '(+ - / *)))

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* ((`(,<.> ,first ,second) formula)
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
  (let ((`(,>?< ,left ,right) equation))
    (cond ((and (is factor used-in? left)
		(is factor used-in? right))
	   `(oops: ,equation))
	  ((is factor used-in? left)
	   (extract factor #;from left #;into right))
	  ((is factor used-in? right)
	   (extract factor #;from right #;into left)))))

(untangle 'e '(= (* a (/ b (+ c (- d e)))) f))

(e.g.
 (untangle 'a #;from '(= (/ a b) (/ c d))) ===> (* (/ c d) b))

(define (factor? x)
  (or (number? x)
      (symbol? x)))

(define (factors formula)
  (if (factor? formula)
      `(,formula)
      (let ((`(,operator . ,operands) formula))
	(apply union (map factors operands)))))


(define-syntax (assert property)
  (unless property
    (error "Assertion failed: " 'property (current-source-location))))

(e.g.
 (factors '(= (/ a b) (/ c d))) ===> (c d b a))

(e.g.
 (factors '(= (+ (* x 5) 7) 0)) ===> (0 7 5 x))

(untangle 'x '(= (+ (* x 5) 7) 0))

(define (variables formula)
  (filter symbol? (factors formula)))

;; Potrzeba troszkę eseistyki.
;; Chcielibyśmy mieć coś takiego, że jak zmienimy wartość zmiennej,
;; to informujemy o tym wszystkich obserwatorów.
;; W związku z tym, każda zmienna powinna mieć listę obserwatorów,
;; zaś każdy obserwator powinien dysponować formułą na ponowne obliczanie
;; jego wartości.

;; Teraz tak. W jaki sposób mielibyśmy użyć naszych asercji?
;; Zakładamy, że mamy sobie funkcję/makro impose, która musi:

;; 1. sprawdzić, jakich zmiennych dotyczy obserwabla
;; 2. zadeklarować rzeczone zmienne

(use-modules (ice-9 local-eval))

(define *observers* (make-hash-table))

(define (observable? var)
  (and (variable? var)
       (hashq-ref *observers* var)))

(define (make-observable! name module #:= (current-module))
  (let ((variable (or (module-variable module name)
		      (make-undefined-variable))))
    (unless (observable? variable)
      (hashq-set! *observers* variable '()))))

(define-syntax (declare variable)
  (make-observable! 'variable))

(make-observable! 'x)

(observable? (module-variable (current-module) 'x))

(let ((x 10))
  (set! x 7)
  (local-eval '(variable? x) (the-environment)))

(eval '(defined? 'x) (current-module))

(module-define! (current-module) 'x 5)

(define-syntax (impose constrain)
  
  
  )

(let ((z 5))
  (defined? 'x))

  
(define (assert x)
  (assert (and (equation? x)
	       (every (lambda (factor)
			(or (location? factor)
			    (number? factor)))
		      (factors x))))
  (let ((locations (filter location? (factors x))))
    ...))



(define inhibited-observers (make-parameter '()))

(define (update! pending-variables+new-values
		 #;remembering updated-variables
			       #;minding conflicting-variables)
  (if (null? pending-variables+new-values)
      conflicting-variables
      (let* ((updated (map (lambda (`(,variable . ,value))
			     (variable-set! variable value)
			     variable)
			   pending-variables+new-values))
	     (updated-variables (union updated updated-variables))
	     (notified (difference (append-map observers updated)
				   conflicting-variables))
	     (changes (filter-map
		       (lambda (observer)
			 (and (isnt observer member conflicting-variables)
			      (let ((new-value (eval (formula observer)
						     (current-module)))
				    (old-value (variable-ref observer)))
				(and (isnt new-value equal? old-value)
				     `(,observer . ,new-value)))))
		       notified))
	     (conflicting-variables (union conflicting-variables
					   (filter-map
					    (lambda (`(,variable . ,value))
						(is variable member
						    updated-variables))
					    changes))))
	(update! changes updated-variables conflicting-variables))))

(define (assign! variable value)
  (let* ((inhibited (inhibited-observers))
	 (observers (difference (observers variable) inhibited)))
    (set-value! variable value)
    (parameterize ((inhibited-observers `(,variable ,@observers ,@inhibited)))
      (for observer in observers
	(inform! observer value variable)))))

(define (inform! target #;about new-value #;of soruce)
  ...)
