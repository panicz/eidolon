(define-module (constraints)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  #:export (keep-updating!)
  #:replace ((assign! . set!))
  #:export-syntax (assign! impose))

(define (operator? x)
  (is x member '(+ - / *)))

(define (used-in? factor formula)
  (or (equal? factor formula)
      (and-let* ((`(,<.> ,first ,second) formula)
		 ((operator? <.>)))
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

(e.g.
 (untangle 'x '(= (+ (* x 5) 7) 0)) ===> (/ (- 0 7) 5))

(define (variables formula)
  (filter symbol? (factors formula)))

(define# (observers variable) '())

(define# (formula variable) #f)

(define# (dependencies variable) '())

(define (possibly-fresh-variable name)
  (let ((module (current-module)))
    (or (module-variable module name)
	(let ((fresh (make-undefined-variable)))
	  (module-define! module name fresh)
	  fresh))))

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
  (for var in (variables 'constrain)
    (let* ((formula (untangle var #;from 'constrain))
	   (value-dependencies (variables formula)))
      (set! (dependencies var) value-dependencies)
      (for dependency in value-dependencies
	(keep-updating! var #;about dependency #;using formula)))))

(define (recalculate variable)
  (when (every defined? (dependencies variable))
    (eval (formula variable) (current-module))))

(define (update! variables+values updated)
  (if (null? variables+values)
      updated
      (let* ((modified (filter-map (lambda (`(,variable . ,value))
				     (and (isnt value equal?
						(variable-ref variable))
					  (variable-set! variable value)
					  variable))
				   variables+values))
	     (updated (union updated modified))
	     (notified (difference (fold-left union '() (map observers modified))
				   updated))
	     (changes (map (lambda (observer)
			     `(,observer . ,(recalculate observer)))
			   notified)))
	(update! changes updated))))

(define-syntax assign*
  (syntax-rules ()
    ((assign* variables+values)
     (assing* variables+values ()))
    
    ((assign* () ((variable value) ...))
     (update!
      `((,(module-variable (current-module) 'variable) . ,value) ...) '()))
    
    ((assign* (variable value . rest) processed)
     (assign* rest ((variable value) . processed)))))

(define-syntax (assign! variable value . rest)
  (assign* (variable value . rest) ()))

(e.g.
 (begin
   (define x 5)
   (define y 1/5)
   (impose (= x (/ 1 y)))
   (assign! x 6)
   y) ===> 1/6)

(e.g.
 (begin
   (define a 2)
   (define b 3)
   (define c 6)
   (impose (= (* a b) c))
   (print (observers a))
   (assign! a 5 b 10)
   c) ===> 50)
