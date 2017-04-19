; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define-datatype assignment assignment?
	(ass-exp
		(var symbol?)
		(val expression?)))

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

(define literal?
	(lambda (x)
		(and (not (pair? x)) (or (number? x) (string? x) (boolean? x) (vector? x)))))
	
(define parse-exp
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
	  ((literal? datum) (lit-exp datum))
      ((pair? datum)
       (if (not (list? datum))
           (eopl:error 'parse-exp "Expression must be a proper list: ~s" datum)
           (cond 
       	     [(eqv? (car datum) 'lambda)
              (if  (> 3 (length datum))
                  (eopl:error 'parse-exp "Too few args to lambda: ~s" datum)
                  (if (symbol? (cadr datum))
                     	(lambda-exp-variable (cadr datum) (map parse-exp (cddr datum)))
                      (if (not (andmap symbol? (cadr datum))) (eopl:error 'parse-exp "Lambda args must be symbols: ~s" datum)
                      (lambda-exp (cadr datum)
                        (map parse-exp (cddr datum))))))]
          		 [(eqv? (car datum) 'if) (if (> 3 (length datum)) (eopl:error 'parse-exp "Too few args to if: ~s" datum)
        		    (if (null? (cdddr datum))
							(if (not (= (length datum) 3)) (eopl:error 'parse-exp "Wrong number of args to if: ~s" datum)

             			  (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))
						  (if (not (= (length datum) 4)) (eopl:error 'parse-exp "Wrong number of args to a if-then: ~s" datum)
             			  (if-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))))]
          		 [(eqv? (car datum) 'set!) (if (not (= 3 (length datum))) (eopl:error 'parse-exp "Error in set!: ~s" datum)
        		    (set!-exp (cadr datum) (parse-exp (caddr datum))))]
          		 [(eqv? (car datum) 'let) (if (> 3 (length datum)) (eopl:error 'parse-exp "No body: ~s" datum) (if (or (not (list? (cadr datum))) (not (andmap list? (cadr datum))) (not (andmap (lambda (x) (= 2 (length x))) (cadr datum))) (not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))) (eopl:error 'parse-exp "Improper let args: ~s" datum) (if (symbol? (cadr datum))
                                     			  (named-let-exp (cadr datum) (map parse-exp (caddr datum)) (map parse-exp (cdddr datum)))
                                     			  (let-exp (car datum) (map parse-exp (cadr datum)) (map parse-exp (cddr datum))))))]
          		 [(or (eqv? 'letrec (car datum)) (eqv? 'let* (car datum))) (if (or (not (list? (cadr datum))) (not (andmap list? (cadr datum))) (not (andmap (lambda (x) (= 2 (length x))) (cadr datum))) (not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))) (eopl:error 'parse-exp "Improper let args: ~s" datum) (if (> 3 (length datum)) (eopl:error 'parse-exp "No body to let ~s" datum)
      		      (let-exp (car datum) (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))))]
         [else (app-exp
                 (parse-exp (car datum))
                 (map parse-exp (cdr datum)))])))
       (else (eopl:error 'parse-exp
               "Invalid syntax ~s" datum)))))

(define unparse-exp 
  (lambda (exp)
    (cases expression exp
      (var-exp (id) id)
	  (lit-exp (lit) lit)
	  (if-exp (pred true-body) (list 'if (unparse-exp pred) (unparse-exp true-body)))
	  (if-else-exp (pred true-body false-body) (list 'if (unparse-exp pred) (unparse-exp true-body) (unparse-exp false-body)))
	  (set!-exp (id body) (list 'set! (id) (unparse-exp body)))
	  (let-exp (id vars body) (cons id (cons (map unparse-exp vars) (map unparse-exp body))))
	  (named-let-exp (name vars body) (cons 'if  (cons name (cons (map unparse-exp vars) (map unparse-exp body)))))
      (lambda-exp (id body) (cons 'lambda (cons id (map unparse-exp body))))
	  (lambda-exp-variable (id body) (cons 'lambda (cons id (map unparse-exp body))))
      (app-exp (rator rand)
        (cons (unparse-exp rator)
              (map unparse-exp rand))))))

(define occurs-free? 
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))
