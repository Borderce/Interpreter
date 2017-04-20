
;; Parsed expression datatypes

(define-datatype expression expression? 
  (var-exp
      (id symbol?))
  (lit-exp
	    (lit literal?))
  (if-exp
	    (pred expression?)
	(true-body expression?))
  (if-else-exp
	    (pred expression?)
	(true-body expression?)
	(false-body expression?))
  (set!-exp
	    (id symbol?)
	(body expression?))
  (let-exp
	    (id symbol?)
	    (vars (list-of (lambda (x) (and (list? x) (symbol? (car x)) (expression? (cadr x))))))
	    (body (list-of expression?)))
  (named-let-exp
      (name symbol?)
	(vars (list-of (lambda (x) (and (list? x) (symbol? (car x)) (expression? (cadr x))))))
	    (body (list-of expression?)))
  (lambda-exp
      (id (list-of symbol?))
      (body (list-of expression?)))
  (lambda-exp-variable
      (id symbol?)
	    (body (list-of expression?)))
  (app-exp
	    (rator expression?)
      (rand (list-of expression?))))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))