(define-module (notifying-assignment)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  #:replace ((notifying-set! . set!))
  #:export (observers inhibited? on-inhibited-location-update)
  #:export-syntax (on-change reference))

(define (assert proposition)
  (let ((result proposition))
    (unless result
      (error "Assertion failed: "'proposition" (got "result")"))))

(define# (observers object) '())

(define-syntax reference ()
  ((reference (procedure ...))
   (list procedure ...))
  ((reference variable)
   (module-variable (current-module) 'variable)))

(define-syntax (on-change location action)
  (assert (procedure? action))
  (let ((target (reference location)))
    (set! (observers target) `(,action . ,(observers target)))))

(define on-inhibited-location-update
  (make-parameter (lambda (location value)
		    (warn "Inhibited update of "location" to "value))))

(define inhibited?
  (make-parameter (lambda# (location) #false)))

(define-syntax (notifying-set! target expression)
  (let ((new-value expression)
	(old-value target)
	(location (reference target)))
    (when (isnt new-value equal? old-value)
      (cond (((inhibited?) location)
	     ((on-inhibited-location-update) 'target new-value))
	    (else
	     (set! target new-value)
	     (set! ((inhibited?) location) #true)
	     (for action in (observers location)
	       (action new-value))
	     (unset! ((inhibited?) location)))))))
