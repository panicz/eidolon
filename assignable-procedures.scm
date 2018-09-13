(define-module (assignable-procedures)
  #:use-module (grand scheme)
  #:export-syntax (define# lambda# unset!))


(define-syntax (lambda# params body . *)
  (let ((default (lambda params body . *))
	(patches (make-hash-table)))
    (with-procedure-properties ((hash-table patches))
      (make-procedure-with-setter
       (lambda args
	 (match (hash-get-handle patches args)
	   (`(,key . ,value)
	    value)
	   (_
	    (apply default args))))
       (lambda args
	 (hash-set! patches (drop-right args 1) (last args)))))))

(define-syntax (define# (mapping . args) body . *)
  (define mapping (lambda# args body . *)))

(define-syntax (unset! (assignable-procedure point ...))
  (hash-remove! (procedure-property assignable-procedure 'hash-table)
		(list point ...))
  (if #f #f))
