; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(literal? datum) (lit-exp datum)]
     [(pair? datum)
      (if (not (list? datum))
		(eopl:error 'parse-exp "expression must be a proper list: ~s" datum)
		(cond 
			[(eqv? (1st datum) 'lambda)
				(if (null? (cddr datum))
					(eopl:error 'parse-exp "too few arguments to lambda expression: ~s" datum)
					(lambda-exp (2nd datum) (map parse-exp (cddr datum))))]
			[(eqv? (1st datum) 'if)
				(if (or	(null? (cdr datum))
						(null? (cddr datum)))
					(eopl:error 'parse-exp "too few cases to if expression: ~s" datum)
					(if (null? (cdddr datum))
						(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
						(if (null? (cddddr datum))
							(if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))
							(eopl:error 'parse-exp "too many cases to if expression: ~s" datum))))]
			[(eqv? (1st datum) 'let)
				(let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
			[(eqv? (1st datum) 'quote)
				(if (or (not (null? (cdr datum)))
						(null? (cddr datum)))
					(quoted-exp (2nd datum))
					(eopl:error 'parse-exp "invalid arguments to quote-exp: ~s" datum))]
			[else (app-exp (parse-exp (1st datum))
				(map parse-exp (cdr datum)))]))]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))



(define literal?
	(lambda (x)
		(and (not (pair? x)) (or (number? x) (string? x) (boolean? x) (vector? x)))))

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
