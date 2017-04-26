; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

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
								(if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))
								(eopl:error 'parse-exp "too many cases to if expression: ~s" datum))))]
					[(eqv? (1st datum) 'let)
						(let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
					[(eqv? (1st datum) 'quote)
						(if (or (not (null? (cdr datum)))
							(null? (cddr datum)))
							(quoted-exp (2nd datum))
							(eopl:error 'parse-exp "invalid arguments to quote-exp: ~s" datum))]
					[(eqv? (1st datum) 'while)]
					[(eqv? (1st datum) 'and)
						(and-exp (map parse-exp (cdr datum)))]
					[(eqv? (1st datum) 'or)
						(or-exp (map parse-exp (cdr datum)))]
					[(eqv? (1st datum) 'let*)]
					[(eqv? (1st datum) 'case)]
					[(eqv? (1st datum) 'cond)]
					[(eqv? (1st datum) 'begin)
						(begin-exp (map parse-exp (cdr datum)))]
					[else (app-exp (parse-exp (1st datum))
						(map parse-exp (cdr datum)))]))]
     	[else (eopl:error 'parse-exp "bad expression: ~s" datum)])))



(define literal?
	(lambda (x)
		(and (not (pair? x)) (or (number? x) (string? x) (boolean? x) (vector? x)))))