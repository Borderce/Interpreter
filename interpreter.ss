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
    [if-else-exp (testCase true-body false-body)
      (if (eval-exp testCase env)
        (eval-exp true-body env)
        (eval-exp false-body env))]
		[lambda-exp (id bodies)
			(closure id bodies env)]
		[quoted-exp (data) data]
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
				; You will add other cases
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
					(cons (car args) (get-args (cdr syms) (cdr args))))))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr list null? assq eq? equal? atom?
                            length list->vector list? pair? procedure? vector->list vector make-vector 
                            vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display 
                            newline caar cadr cddr cdar caaar cadar cdddr caddr caadr cdaar cddar cdadr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
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
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply append args '())]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector) (apply vector args)]
      [(vector->list) (vector->list (1st args))]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cddr) (cddr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(cadar) (cadar (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define syntax-expand
	(lambda (expr)
		(cases expression expr
			[lit-exp (datum) expr]
			[var-exp (id) expr]
			[lambda-exp (id bodies)
				(lambda-exp id (map syntax-expand bodies))]
			[if-else-exp (test true-body false-body)
				(if-else-exp (syntax-expand test) (syntax-expand true-body) (syntax-expand false-body))]
			[let-exp (vars vals bodies)
				(app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand vals))]
			[quoted-exp (data) expr]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" expr)])))

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