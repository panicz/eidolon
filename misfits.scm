
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
