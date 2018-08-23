(define-module (notifying-assignment)
  #:use-module (grand scheme)
  #:use-module (assignable-procedures)
  #:replace ((notifying-set! . set!))
  #:export-syntax (on-change))

(define# (observers object) '())

(define-syntax on-change
  (lambda (stx)
    (syntax-case stx ()
      ((on-change (procedure arguments ...) action)
       (with-syntax (((parameters ...)
		      (generate-temporaries #'(arguments ...))))
	 #'(let ((target procedure)
		 (parameters arguments)
		 ...)
	     (let ((event-source `(,target ,parameters ...)))
	       (set! (observers event-source)
		     `(,action . ,(observers event-source)))))))
       
      ((on-change variable action)
       (identifier? #'object)
       #'(let ((location (module-variable (current-module) 'variable)))
	   (set! (observers location) `(,action . ,(observers location))))))))


(define-syntax notifying-set!
  (lambda (stx)
    (syntax-case stx ()
      ((notifying-set! (procedure arguments ...) expression)
       (with-syntax (((parameters ...)
		      (generate-temporaries #'(arguments ...))))
	 #'(let ((value expression)
		 (target procedure)
		 (parameters arguments)
		 ...)
	     (let ((notifications (observers `(,target ,parameters ...))))
	       (set! (procedure arguments ...) value)
	       (for deliver in notifications
		 (deliver value))))))
      
      
      ((notifying-set! variable expression)
       (identifier? #'variable)
       #'(let* ((value expression)
		(location (module-variable (current-module) 'variable))
		(notifications (observers location)))
	   (variable-set! location value)
	   (for deliver in notifications
	     (deliver value)))))))
