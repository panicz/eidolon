# Eidolon -- ideas

- Eidolon to Scheme transformation, then compile it
  with Chez? (initially)
- optimizing data structures (vide master thesis)
- powerful assertions/constraints used for:
  - polymorphism/types/partial definitions, for example
    ```
    (define (apply symbol (dictionary))
      (assert (and (symbol? symbol)
                   (associacion? dictionary)))
      (lookup symbol dictionary))

      (e.g.
	('b '((a 1) (b 2) (c 3))) ===> 2)

    (define (apply dictionary symbols)
      (assert (every symbol? symbols))
      (fold-left (lambda (dictionary symbol)
                   (lookup symbol dictionary))
		 dictionary
		 symbol))
    ```
  - signal propagation -- for example `(assert (= (/ a b) (/ c d)))`
    should make sure, for example, than whenever `a` is changed,
    `b` is set to `(/ (* a d) c)`, `c` -- to `(* a (/ d b))` and
    `d` -- to `(/ (* c b) a)`

  - defining new types, for example
    ```
    (define (matrix? x)
      (and (list? x)
           (every list? x)
           (apply = (map length x))))
    (assert (distinct? matrix?))
    ```
- Hindley-Milner type inference/unification
  (may turn out helpful in the context of list recursion
  from chaper 6 of the master thesis)

- built-in "backwards-evaluator" from minikanren
  (for things like finding inverse functions without having
  to actually write any line of code, and other neat tricks)

- theorem prover (vide chapter 5 of master thesis)

How to differentiate between various classes of assertions?
* polymorphism -- used inside of a lambda-expression
* slots&signals -- solvable equation
* distinct types -- used with "distinct?" predicate
