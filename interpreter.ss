; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (expr env)
    (cases expression expr
		[lit-exp (datum) datum]
		[var-exp (id)
			(apply-env env id ; look up its value.
				(lambda (x) x) ; procedure to call if id is in the environment
				(lambda ()
					(apply-env global-env id (lambda (x) x)
					(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s"
						id)))))]
		[lambda-exp (id bodies)
			(closure id bodies env)]
		[if-exp (test true-body)
			(if (eval-exp test env)
				(eval-exp true-body env))]
		[if-else-exp (test true-body false-body)
			(if (eval-exp test env)
				(eval-exp true-body env)
				(eval-exp false-body env))]
		[quoted-exp (data) data]
		[while-exp (test bodies)
			(let whileLoop ()
				(if (eval-exp test env)
					(let innerLoop ([bodies bodies])
						(if (null? bodies)
							(whileLoop)
							(begin
								(eval-exp (car bodies) env)
								(innerLoop (cdr bodies)))))))]
		[letrec-exp (proc-names idss bodies letrec-bodies)
			(let ([new-env (extend-env-recursively
								proc-names idss bodies env)])
			(let loop ([bodies letrec-bodies])
				(if (null? (cdr bodies))
					(eval-exp (1st bodies) new-env)
					(begin (eval-exp (1st bodies) new-env) (loop (cdr bodies))))))]
		[app-exp (rator rands)
			(let ([proc-value (eval-exp rator env)]
				[args (eval-rands rands env)])
			(apply-proc proc-value args))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" expr)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (e) (eval-exp e env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args)]
		[closure (params body env)
			(let ([syms-args (get-syms-and-args params args)])
				(let ([extended-env (extend-env
										(if (null? syms-args)
											'()
											(car syms-args))
										(if (null? syms-args)
											'()
											(cdr syms-args))
										env)])
					(let loop ([bodies body])
						(if (null? (cdr bodies))
							(eval-exp (1st bodies) extended-env)
							(begin
								(eval-exp (1st bodies) extended-env)
								(loop (cdr bodies)))))))]
		[else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)])))

(define get-syms-and-args
	(lambda (syms args)
		(cond
			[(null? syms) '()]
			[(list? syms)
				(cons syms args)]
			[(atom? syms)
				(cons (list syms) (list args))]
			[else
				(cons (get-syms syms) (get-args syms args))])))

(define get-syms
	(lambda (syms)
		(if (null? syms)
			'()
			(if (null? (cdr syms))
				syms
				(if (pair? (cdr syms))
					(cons (car syms) (get-syms (cdr syms)))
					(cons (car syms) (list (cdr syms))))))))

(define get-args
	(lambda (syms args)
		(if (null? args)
			'()
			(if (null? (cdr syms))
				args
				(if (pair? (cdr syms))
					(cons (car args) (get-args (cdr syms) (cdr args)))
					(cons (car args) (list (cdr args))))))))


(define *prim-proc-names*
	'(+ - * / add1 sub1 = < > <= >= zero? not cons car cdr list null? assq eq? eqv? equal? atom? length
		list->vector list? pair? procedure? vector->list vector make-vector vector-ref
		vector? number? symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar
		cddr caaar caadr cadar cdaar caddr cdadr cdaar cdddr void quotient apply map list-tail append))



(define init-env       	; for now, our initial global environment only contains
  (extend-env         	; procedure names.  Recall that an environment associates
     *prim-proc-names*	;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
		(empty-env)))
		
(define global-env init-env)


; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
	  [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
	  [(=) (= (1st args) (2nd args))]
	  [(<) (< (1st args) (2nd args))]
	  [(>) (> (1st args) (2nd args))]
	  [(<=) (<= (1st args) (2nd args))]
	  [(>=) (>= (1st args) (2nd args))]
	  [(zero?) (zero? (1st args))]
	  [(not) (not (1st args))]
      [(cons) (cons (1st args) (2nd args))]
	  [(car) (car (1st args))]
	  [(cdr) (cdr (1st args))]
	  [(list) args]
	  [(null?) (null? (1st args))]
	  [(assq) (assq (1st args) (2nd args))]
	  [(eq?) (eq? (1st args) (2nd args))]
	  [(eqv?) (eqv? (1st args) (2nd args))]
	  [(equal?) (equal? (1st args) (2nd args))]
	  [(atom?) (atom? (1st args))]
	  [(length) (length (1st args))]
	  [(list->vector) (list->vector (1st args))]
	  [(list-tail) (list-tail (1st args) (2nd args))]
	  [(list?) (list? (1st args))]
	  [(pair?) (pair? (1st args))]
	  [(procedure?) (proc-val? (1st args))]
	  [(vector->list) (vector->list (1st args))]
	  [(vector) (apply vector args)]
	  [(make-vector) (apply make-vector (args))]
	  [(vector-ref) (vector-ref (1st args) (2nd args))]
	  [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
	  [(vector?) (vector? (1st args))]
	  [(number?) (number? (1st args))]
	  [(symbol?) (symbol? (1st args))]
	  [(set-car!) (set-car! (1st args) (2nd args))]
	  [(set-cdr!) (set-cdr! (1st args) (2nd args))]
	  [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caar) (caar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
	  [(quotient) (quotient (1st args) (2nd args))]
	  [(apply) (my-apply (1st args) (2nd args))]
	  [(map) (my-map (1st args) (2nd args))]
	  [(void) (void)]
	  [(display) (display (1st args))]
	  [(newline) (newline)]
	  [(append) (apply append args)]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-proc)])))
			
(define my-apply
	(lambda (proc args)
		(cases proc-val proc
			[prim-proc (name) 
				(apply-prim-proc name args)]
			[closure (params procedure env) 
				(apply-proc proc args)]
			[else
				(error 'my-apply "D: ~s" proc)])))

(define my-map
	(lambda (proc args)
		(cases proc-val proc
			[prim-proc (name)
				(let loop ((args args))
					(if (null? args)
						'()
						(cons (apply-prim-proc name (list (1st args))) (loop (cdr args)))))]
			[closure (params body env)
				(let loop ((args args))
					(if (null? args)
						'()
						(cons (apply-proc proc (list (1st args))) (loop (cdr args)))))]
			[else
				(error 'my-map "D: ~s" proc)])))
			
(define syntax-expand
	(lambda (expr)
		(cases expression expr
			[lit-exp (datum) expr]
			[var-exp (id) expr]
			[lambda-exp (id bodies)
				(lambda-exp id (map syntax-expand bodies))]
			[if-exp (test true-body)
				(if-exp (syntax-expand test) (syntax-expand true-body))]
			[if-else-exp (test true-body false-body)
				(if-else-exp (syntax-expand test) (syntax-expand true-body) (syntax-expand false-body))]
			[let-exp (vars vals bodies)
				(app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand vals))]
			[quoted-exp (data) expr]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[and-exp (bodies)
				(let loop ((bodies bodies))
						(if (null? bodies)
							(lit-exp #t)
							(if (null? (cdr bodies))
								(syntax-expand (car bodies))
								(if-else-exp (syntax-expand (car bodies)) (loop (cdr bodies)) (lit-exp #f)))))]
			[or-exp (bodies)
				(let loop ((bodies bodies))
						(if (null? bodies)
							(lit-exp #f)
							(if (null? (cdr bodies))
								(syntax-expand (car bodies))
								(syntax-expand (let-exp (list 'TylerJ) (list (syntax-expand (car bodies)))
									(list (if-else-exp (var-exp 'TylerJ) (var-exp 'TylerJ) (loop (cdr bodies)))))))))]
			[begin-exp (bodies) (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
			[let*-exp (vars vals bodies) (car (let loop ([vars vars] [vals vals])
												(if (null? vars)
													(map syntax-expand bodies)
													(list (app-exp (lambda-exp (list (car vars)) (loop (cdr vars) (cdr vals)))  
																	(list(syntax-expand (car vals))))))))]
			[letrec-exp (proc-names idss bodies letrec-bodies)
				(letrec-exp proc-names idss (map syntax-expand bodies) (map syntax-expand letrec-bodies))]
			[named-let-exp (name vars vals bodies)
				(syntax-expand (letrec-exp (list name) (list vars) bodies (list (app-exp (var-exp name) vals))))]
			[while-exp (test bodies)
				(while-exp (syntax-expand test) (map syntax-expand bodies))]
			[cond-exp (tests bodies)
				(let loop ([tests tests] [bodies bodies])
					(if (null? tests) 
						(app-exp (var-exp 'void) '())
						(if (eqv? (cadr (car tests)) 'else)
							(syntax-expand (car bodies))
							(if-else-exp (syntax-expand (car tests)) (syntax-expand (car bodies)) (loop (cdr tests) (cdr bodies))))))]
			[case-exp (expr keys bodies)
				(syntax-expand (cond-exp (case-expand-keys expr keys) (case-expand-bodies keys bodies)))]
			[else (eopl:error 'syntax-expand "Bad abstract syntax: ~a" expr)])))
			
(define case-expand-keys
	(lambda (expr keys)
		(apply append
			(map (lambda (ls) 
					(if (list? (cadr ls)) 
						(map (lambda (key) (app-exp (var-exp 'eq?) (list (parse-exp key) expr))) (cadr ls))
						(list ls)))
				keys))))
		
(define case-expand-bodies
	(lambda (keys bodies)
		(apply append
			(map (lambda (key-list body) 
					(if (list? (cadr key-list)) 
						(map (lambda (x) body) (cadr key-list))
						(list body)))
				keys bodies))))
		
			
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
