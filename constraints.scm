(define-module (constraints)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  

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

(define# (observers variable) '())

(define# (formula variable) #f)

(define# (dependencies variable) '())

(define (possibly-fresh-variable name)
  (let ((module (current-module)))
    (or (module-variable module name)
	(let ((fresh (make-undefined-variable)))
	  (module-define! module name fresh)))))

(define (keep-updating! observer #;about source #;using recipe)
  ;;(assert (and (symbol? observer?) (symbol? source)))
  (let* ((observer-variable (possibly-fresh-variable observer))
	 (source-variable (possibly-fresh-variable source))
	 (source-observers (observers source-variable))
	 (old-recipe (formula observer-variable))) 
    (unless (is observer-variable member source-observers)
      (set! (observers source-variable) `(,observer-variable
					  . , source-observers)))
    (when (and old-recipe (isnt recipe equal? old-recipe))
      (warn "Overwriting" old-recipe "with" recipe "in" observer))
    (set! (formula observer-variable) recipe)))

(define-syntax (impose constrain)
  (for var in (variables constrain)
    (let* ((formula (untangle var #;from constrain))
	   (value-dependencies (variables formula)))
      (set! (dependencies var) value-dependencies)
      (for dependency in value-dependencies
	(keep-updating! var #;about dependency #;using formula)))))

(define (recalculate variable)
  (when (every defined? (dependencies variable))
    (eval (formula variable) (current-module))))

(define (update! variables+values updated)
  (unless (null? variables+values)
    (let* ((modified (filter-map (lambda (`(,variable . ,value))
				   (and (isnt value equal?
					      (variable-ref variable))
					(variable-set! variable value)
					variable))
				 variables+values))
	   (updated (union updated modified))
	   (notified (fold-left union '() (filter (isnt _ memq updated)
						  (map observers modified))))
	   (changes (map (lambda (observer)
			   `(,observer . ,(recalculate observer)))
			 notified)))
      (update! changes updated))))

(define-syntax (assign! variable value)
  (update! `(,(module-ref (current-module) 'variable) . ,value) '()))
