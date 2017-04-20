
;; Parsed expression datatypes

(define-datatype expression expression? 
  [var-exp
      (id symbol?)]
  [lit-exp
	    (lit literal?)]
  [if-exp
	    (pred expression?)
	    (true-body expression?)]
  [if-else-exp
	    (pred expression?)
	    (true-body expression?)
	    (false-body expression?)]
  [quoted-exp
    (data scheme-value?)]
  [set!-exp
	    (id symbol?)
	    (body expression?)]
  [let-exp
	    (id symbol?)
	    (vars (list-of (lambda (x) (and (list? x) (symbol? (car x)) (expression? (cadr x))))))
	    (body (list-of expression?))]
  [named-let-exp
      (name symbol?)
	    (vars (list-of (lambda (x) (and (list? x) (symbol? (car x)) (expression? (cadr x))))))
	    (body (list-of expression?))]
  [lambda-exp
      (id (list-of symbol?))
      (body (list-of expression?))]
  [lambda-exp-variable
      (id symbol?)
	    (body (list-of expression?))]
  [app-exp
	    (rator expression?)
      (rand (list-of expression?))])

(define literal?
	(lambda (x)
		(ormap
			(lambda (pred) (pred x))
				(list number? vector? boolean? symbol? string? null?))))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
   [closure
		(params (lambda (x) (or (list-of symbol? x) (symbol? x) (check-syms x))))
		(body (list-of expression?))
		(env environment?)])
	 
(define check-syms
  (lambda (syms)
    (if (null? syms)
	#t
	(if (pair? (cdr syms))
	    (if (symbol? (car syms))
		(check-syms (cdr syms))
		#f)
	    (if (symbol? (car syms))
		(symbol? (cdr syms))
		#f)))))	 
	 
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (lambda (x) (or (pair? x) (symbol? x) (null? x))))
   (vals (list-of scheme-value?))
   (env environment?)))