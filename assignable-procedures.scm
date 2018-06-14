(define-module (assignable-procedures)
  #:use-module (grand scheme)
  #:export-syntax (define# lambda#))

(define-syntax (lambda# params body . *)
  (let ((default (lambda params body . *))
	(patches (make-hash-table)))
    (make-procedure-with-setter
     (lambda args
       (match (hash-get-handle patches args)
	 (`(,key . ,value)
	  value)
	 (_
	  (apply default args))))
     (lambda args
       (hash-set! patches (drop-right args 1) (last args))))))

(define-syntax (define# (mapping . args) body . *)
  (define mapping (lambda# args body . *)))
