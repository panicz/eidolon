(define-module (notifying-assignment)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  #:replace ((notifying-set! . set!))
  #:export-syntax (on-change))

(define# (observers object) '())

(define-syntax reference ()
  ((reference (procedure ...))
   (list procedure ...))
  ((reference variable)
   (module-variable (current-module) 'variable)))

(define-syntax (on-change location action)
  (let ((target (reference location)))
    (set! (observers target) `(,action . ,(observers target)))))

(define-syntax (notifying-set! target expression)
  (let ((value expression)
	(location (reference target)))
    (for action in (observers location)
      (action value))))
