
;; Parsed expression datatypes

(define-datatype expression expression?
	[var-exp        ; variable references
		(id symbol?)]
	[lit-exp        ; "Normal" data.  Did I leave out any types?
		(datum literal?)]
	[lambda-exp 
		(id (lambda (x) (or (pair? x) (symbol? x) (null? x))))
		(bodies (list-of expression?))]
	[if-exp
		(test expression?)
		(true-body expression?)]
	[if-else-exp
		(test expression?)
		(true-body expression?)
		(false-body expression?)]
	[let-exp
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))]
	[quoted-exp
		(data scheme-value?)]
	[app-exp        ; applications
		(rator expression?)
		(rands (list-of expression?))])

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