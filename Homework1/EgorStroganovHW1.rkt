 #lang racket

; -----------------------Exercise 1.1-----------------------

; predicate that returns #t if x is mathematical
; operation otherwise #f
; Example:
; (operation? '+) -> #t
; (operation? 'x) -> #f
(define (operation? x)
  (if (member x '(+ - * / sin cos tan log exp)) #t #f))

; predicate that returns #t if x is symbolic variable
; or list which contain single element that
; variable otherwise #f
; Example:
; (variable? '+)     -> #f
; (variable? 'x)     -> #t
; (variable? '(var)) -> #t
(define (variable? expr)
  (cond
    [(list? expr)
     (if (= 1 (length expr))
         (if (and (symbol? (first expr)) (not (operation? (first expr)))) #t #f)
         #f)]
    [else (if (and (symbol? expr) (not (operation? expr))) #t #f)]))

; predicate that returns #t if x is
; list or variable or number
; otherwise #f
; Example:
; (term? '+)       -> #f
; (term? '(+ 1 x)) -> #t
; (term? '4)       -> #t
(define (term? x)
  (or (list? x) (number? x) (variable? x)))

; predicate that returns #t if x is
; variable or number
; otherwise #f
; Example:
; (simple-term? 'z)       -> #t
; (simple-term? '(+ 1 x)) -> #f
; (simple-term? '4)       -> #t
(define (simple-term? x)
  (or (number? x) (variable? x)))

; predicate that returns #t if expr is
; sum in form (+ term1 term2)
; otherwise #f
; Example:
; (sum? '(+ 1 2))       -> #t
; (sum? '(+ (* 2 y) x)) -> #t
; (sum? '(* x y))       -> #f
(define (sum? expr)
  (if (list? expr)
      (cond
    [(= 3 (length expr)) (cond
        [(term? (second expr)) (cond
            [(term? (third expr)) (if (equal? '+ (first expr)) #t #f)]
            [else #f])]
        [else #f])]
    [else #f])
      (error "sum? : input is not expression" expr)))

; returns 1st operand of sum
; Example:
; (summand-1 '(+ x y))             -> 'x
; (summand-1 '(+ (* z y) (+ 1 y))) -> '(* z y)
(define (summand-1 expr)
  (if (sum? expr) (second expr)
      (error "summand-1 : expression is not sum " expr)))

; returns 2nd operand of sum
; Example:
; (summand-2 '(+ x y))             -> 'y
; (summand-2 '(+ (* z y) (+ 1 y))) -> '(+ 1 y)
(define (summand-2 expr)
  (if (sum? expr) (third expr)
      (error "summand-2 : expression is not sum " expr)))

; predicate that returns #t if expr is
; product in form (* term1 term2)
; otherwise #f
; Example:
; (product? '(+ 1 2))       -> #f
; (product? '(* (* 2 y) x)) -> #t
; (product? '(* 1 2))       -> #t
(define (product? expr)
  (if (list? expr)
      (cond
    [(= 3 (length expr)) (cond
        [(term? (second expr)) (cond
            [(term? (third expr)) (if (equal? '* (first expr)) #t #f)]
            [else #f])]
        [else #f])]
    [else #f])
      (error "product? : input is not expression" expr))) 

; returns 1st operand of product
; Example:
; (multiplier-1 '(* x y))             -> 'x
; (multiplier-1 '(* (* z y) (+ 1 y))) -> '(* z y)
(define (multiplier-1 expr)
  (if (product? expr) (second expr)
      (error "multiplier-1 : expression is not product " expr)))

; returns 2nd operand of product
; Example:
; (multiplier-2 '(* x y))             -> 'y
; (multiplier-2 '(* (* z y) (+ 1 y))) -> '(+ 1 y)
(define (multiplier-2 expr)
  (if (product? expr) (third expr)
      (error "multiplier-2 : expression is not product " expr)))

; -----------------------Exercise 1.2-----------------------

; differentiates simple term(variable, number)
; Example:
; (derivate-term 'x 'x)  -> 1
; (derivate-term '23 'x) -> 0
(define (derivate-term term var)
  (if (equal? term var) 1 0))

; constructs list of sum derivative
; (a + b)' = a' + b'
; Example:
; (derivate-sum-to-list x 4) -> '(+ x 4)
(define (derivate-sum-to-list term1 term2)
  (cons '+ (list term1 term2)))

; constructs list of product derivative
; (u * v)' = u'v + uv' 
; Example:
; (derivate-product-to-list x 1 4 0) -> '(+ (* 1 4) (* x 0))
(define (derivate-product-to-list term1 term1-derivate term2 term2-derivate)
  (cons '+ (list (cons '* (list term1-derivate term2)) (cons '* (list term1 term2-derivate)))))

; computes a symbolic derivative of a given
; expression with respect to a given variable
; Example:
; (derivative '(+ 1 x) 'x) -> '(+ 0 1)
; (derivative '(* 2 y) 'y) -> '(+ (* 0 y) (* 2 1))
; (derivative '(* (+ x y) (+ x (+ x x))) 'x) ->
; '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))
(define (derivative expr var)
  (cond
    [(simple-term? expr) (derivate-term expr var)]
    [(sum? expr) (derivate-sum-to-list (derivative (summand-1 expr) var) (derivative (summand-2 expr) var))]
    [(product? expr)
     (derivate-product-to-list
      (multiplier-1 expr) (derivative (multiplier-1 expr) var)
      (multiplier-2 expr) (derivative (multiplier-2 expr) var))]))

; -----------------------Exercise 1.3-----------------------

; simplifies an expression (recursion depth = 1)
; Example:
; (simplify-at-root '(+ (* 0 y) (* 2 1))) ->
; '(+ 0 2)
(define (simplify-at-root expr)
  (if (list? expr) (cond
    [(sum? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (+ (second expr) (third expr))]
                   [(equal? 0 (second expr)) (simplify-at-root (third expr))]
                   [(equal? 0 (third expr)) (simplify-at-root (second expr))]
                   [else (cons '+ (list (simplify-at-root (second expr)) (simplify-at-root (third expr))))])]
    [(product? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (* (second expr) (third expr))]
                   [(equal? 1 (second expr)) (simplify-at-root (third expr))]
                   [(equal? 1 (third expr)) (simplify-at-root (second expr))]
                   [(equal? 0 (second expr)) 0]
                   [(equal? 0 (third expr)) 0]
                   [else (cons '* (list (simplify-at-root (second expr)) (simplify-at-root (third expr))))])]
    )
      expr))

; simplifies the expression until it changes,
; uses simplify-at-root function
; Example:
; (simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))
; -> '(+ (+ x (+ x x)) (* (+ x y) 3))
(define (simplify expr)
  (if (equal? expr (simplify-at-root expr)) expr
      (simplify (simplify-at-root expr))))

; -----------------------Exercise 1.5-----------------------

; converts an expression from prefix form to infix form
; Example:
; (to-infix '(+ (+ x (+ x x)) (* (+ x y) 3))
; -> '((x + (x + x)) + ((x + y) * 3)
(define (to-infix expr)
  (cond
    [(simple-term? expr) expr]
    [(operation? (first expr))
     (list (to-infix (second expr)) (first expr) (to-infix (third expr)))]
    [else expr]))

; ----------------------Exercise 1.6.1----------------------

; predicate that returns #t if expr is
; exponential in form (exp term1 term2)
; otherwise #f
; Example:
; (exp? '(exp 1 2))     -> #t
; (exp? '(+ (* 2 y) x)) -> #f
; (exp? '(log x))       -> #f
(define (exp? expr)
  (if (list? expr)
      (cond
    [(= 3 (length expr)) (cond
        [(term? (second expr)) (cond
            [(term? (third expr)) (if (equal? 'exp (first expr)) #t #f)]
            [else #f])]
        [else #f])]
    [else #f])
      (error "exp? : input is not expression" expr)))

; predicate that returns #t if expr is
; sin in form (sin term)
; otherwise #f
; Example:
; (sin? '(exp 1 2))      -> #f
; (sin? '(+ (* 2 y) x))  -> #f
; (sin? '(sin (/ pi 2))) -> #t
(define (sin? expr)
  (if (list? expr)
      (if (= 2 (length expr))
          (if (equal? 'sin (first expr))
              (if (term? (second expr))
                  #t
                  (error "sin? : invalid argument " (second expr)))
              #f)
          #f)
      (error "sin? : input is not expression" expr)))

; predicate that returns #t if expr is
; cos in form (cos term)
; otherwise #f
; Example:
; (cos? '(exp 1 2))      -> #f
; (cos? '(+ (* 2 y) x))  -> #f
; (cos? '(cos (/ pi 2))) -> #t
(define (cos? expr)
  (if (list? expr)
      (if (= 2 (length expr))
          (if (equal? 'cos (first expr))
              (if (term? (second expr))
                  #t
                  (error "cos? : invalid argument " (second expr)))
              #f)
          #f)
      (error "cos? : input is not expression" expr)))

; predicate that returns #t if expr is
; tan in form (tan term)
; otherwise #f
; Example:
; (tan? '(exp 1 2))      -> #f
; (tan? '(+ (* 2 y) x))  -> #f
; (tan? '(tan (/ x y)))  -> #t
(define (tan? expr)
  (if (list? expr)
      (if (= 2 (length expr))
          (if (equal? 'tan (first expr))
              (if (term? (second expr))
                  #t
                  (error "tan? : invalid argument " (second expr)))
              #f)
          #f)
      (error "tan? : input is not expression" expr)))

; predicate that returns #t if expr is
; natural logarithm in form (log term)
; otherwise #f
; Example:
; (log? '(exp 1 2))      -> #f
; (log? '(+ (* 2 y) x))  -> #f
; (log? '(log x))        -> #t
(define (log? expr)
  (if (list? expr)
      (if (= 2 (length expr))
          (if (equal? 'log (first expr))
              (if (term? (second expr))
                  #t
                  (error "log? : invalid argument " (second expr)))
              #f)
          #f)
      (error "log? : input is not expression" expr)))

; constructs list of sin derivative
; (sin f(x))' = f'(x) * cos(f(x))
; Example:
; (derivate-sin 'x 1) -> '(cos x)
(define (derivate-sin term term-derivate)
  (list '* term-derivate (list 'cos term)))

; constructs list of cos derivative
; (cos f(x))' = -f'(x) * sin(f(x))
; Example:
; (derivate-cos 'x 1) -> '(* -1 (sin x))
(define (derivate-cos term term-derivate)
  (list '* (list '* -1 term-derivate) (list 'sin term)))

; constructs list of tan derivative
; (tan f(x))' = f'(x) / cos^2(f(x))
; Example:
; (derivate-tan 'x 1) -> '(/ 1 (exp (cos x) 2))
(define (derivate-tan term term-derivate)
  (list '/ term-derivate (list 'exp (list 'cos term) 2)))

; constructs list of log derivative
; (log f(x))' = f'(x) / f(x)
; Example:
; (derivate-log 'x 1) -> '(/ 1 x)
(define (derivate-log term term-derivate)
  (list '/ term-derivate term))

; constructs list of exp derivative
; (exp f(x) u(x))' = some long formula...
; Example:
; (derivate-exp 'x 2 1 0) ->
; '(+ (* (* 2 (exp x (- 2 1))) 1) (* (exp x 2) (* (log x) 0)))
(define (derivate-exp u v u-d v-d)
  (list '+ (list '* (list '* v (list 'exp u (list '- v 1))) u-d) (list '* (list 'exp u v) (list '* (list 'log u) v-d))))

; upgrade of derivative. now support log, sin, cos, tan, exp
; Example:
; (derivative-upd1 '(+ x (cos x)) 'x) -> '(+ 1 (* (* -1 1) (sin x)))
(define (derivative-upd1 expr var)
  (cond
    [(simple-term? expr) (derivate-term expr var)]
    [(sum? expr) (derivate-sum-to-list (derivative-upd1 (summand-1 expr) var) (derivative-upd1 (summand-2 expr) var))]
    [(product? expr)
     (derivate-product-to-list
      (multiplier-1 expr) (derivative-upd1 (multiplier-1 expr) var)
      (multiplier-2 expr) (derivative-upd1 (multiplier-2 expr) var))]
    [(sin? expr) (derivate-sin (second expr) (derivative-upd1 (second expr) var))]
    [(cos? expr) (derivate-cos (second expr) (derivative-upd1 (second expr) var))]
    [(tan? expr) (derivate-tan (second expr) (derivative-upd1 (second expr) var))]
    [(log? expr) (derivate-log (second expr) (derivative-upd1 (second expr) var))]
    [(exp? expr)
     (derivate-exp (second expr) (third expr)
                   (derivative-upd1 (second expr) var)
                   (derivative-upd1 (third expr) var))]))

; ----------------------Exercise 1.6.2----------------------

; predicate that returns #t if expr is
; subtraction in form (- term1 term2)
; otherwise #f
; Example:
; (sub? '(- 1 2))       -> #t
; (sub? '(+ (* 2 y) x)) -> #f
; (sub? '(* x y))       -> #f
(define (sub? expr)
  (if (list? expr)
      (cond
    [(= 3 (length expr)) (cond
        [(term? (second expr)) (cond
            [(term? (third expr)) (if (equal? '- (first expr)) #t #f)]
            [else #f])]
        [else #f])]
    [else #f])
      (error "sub? : input is not expression" expr)))

; predicate that returns #t if expr is
; division in form (/ term1 term2)
; otherwise #f
; Example:
; (div? '(- 1 2))       -> #f
; (div? '(+ (* 2 y) x)) -> #f
; (div? '(/ x y))       -> #t
(define (div? expr)
  (if (list? expr)
      (cond
    [(= 3 (length expr)) (cond
        [(term? (second expr)) (cond
            [(term? (third expr)) (if (equal? '/ (first expr)) #t #f)]
            [else #f])]
        [else #f])]
    [else #f])
      (error "div? : input is not expression" expr)))

(define (simplify-upd1-at-root expr)
  (if (list? expr) (cond
    [(sum? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (+ (second expr) (third expr))]
                   [(equal? 0 (second expr)) (simplify-upd1-at-root (third expr))]
                   [(equal? 0 (third expr)) (simplify-upd1-at-root (second expr))]
                   [else (cons '+ (list (simplify-upd1-at-root (second expr)) (simplify-upd1-at-root (third expr))))])]
    [(sub? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (- (second expr) (third expr))]
                   [(equal? 0 (second expr)) (* -1 (simplify-upd1-at-root (third expr)))]
                   [(equal? 0 (third expr)) (simplify-upd1-at-root (second expr))]
                   [(equal? (second expr) (third expr)) 0]
                   [else (cons '- (list (simplify-upd1-at-root (second expr)) (simplify-upd1-at-root (third expr))))])]
    [(product? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (* (second expr) (third expr))]
                   [(equal? 1 (second expr)) (simplify-upd1-at-root (third expr))]
                   [(equal? 1 (third expr)) (simplify-upd1-at-root (second expr))]
                   [(equal? 0 (second expr)) 0]
                   [(equal? 0 (third expr)) 0]
                   [else (cons '* (list (simplify-upd1-at-root (second expr)) (simplify-upd1-at-root (third expr))))])]
    [(div? expr) (cond
                   [(and (number? (second expr)) (number? (third expr)))
                    (/ (second expr) (third expr))]
                   [(equal? 1 (third expr)) (simplify-upd1-at-root (second expr))]
                   [(equal? 0 (second expr)) 0]
                   [(equal? 0 (third expr))
                    (error "simplify-upd1-at-root : division by zero" expr)]
                   [(equal? (second expr) (third expr)) 1]
                   [else (cons '/ (list (simplify-upd1-at-root (second expr)) (simplify-upd1-at-root (third expr))))])]
    [(sin? expr) (cond
                   [(number? (second expr)) (sin (second expr))]
                   [(variable? (second expr)) (list 'sin (second expr))]
                   [else (list 'sin (simplify-upd1-at-root (second expr)))])]
    [(cos? expr) (cond
                   [(number? (second expr)) (cos (second expr))]
                   [(variable? (second expr)) (list 'cos (second expr))]
                   [else (list 'cos (simplify-upd1-at-root (second expr)))])]
    [(tan? expr) (cond
                   [(number? (second expr))
                    (if (= (second expr) (/ pi 2))
                        (error "simplify-upd1-at-root : invalid argument of tan " (second expr))
                        (tan (second expr)))]
                   [(variable? (second expr)) (list 'tan (second expr))]
                   [else (list 'tan (simplify-upd1-at-root (second expr)))])]
    [(log? expr) (cond
                   [(number? (second expr))
                    (if (<= (second expr) 0)
                        (error "simplify-upd1-at-root : argument of log must be > 0 but got" (second expr))
                        (log (second expr)))]
                   [(variable? (second expr)) (list 'log (second expr))]
                   [else (list 'log (simplify-upd1-at-root (second expr)))])]
    [(exp? expr) (cond
                   [(and (number? (second expr)) (number? (third expr))) (expt (second expr) (third expr))]
                   [(equal? 0 (third expr)) 1]
                   [(equal? 1 (third expr)) (second expr)]
                   [else (cons 'exp (list (simplify-upd1-at-root (second expr)) (simplify-upd1-at-root (third expr))))])]
    )
      expr))

; upgrade of simplify. now support exp, log, sin, cos, tan
; Example:
; (simplify-upd1 '(log (sin (cos 100)))) -> -0.2752879404366782
(define (simplify-upd1 expr)
  (if (equal? expr (simplify-upd1-at-root expr)) expr
      (simplify-upd1 (simplify-upd1-at-root expr))))

; -----------------------Exercise 1.7-----------------------

; converts from polyvariadic to binary expression
; Example:
; (to-binary-expr '(+ 1 2 (* 3 4 5) (+ 6 7))) ->
; '(+ (+ (+ 1 2) (* (* 3 4) 5)) (+ 6 7))
(define (to-binary-expr expr)
  (cond
    [(simple-term? expr) expr]
    [(= (length expr) 3)
     (list (first expr) (to-binary-expr (second expr)) (to-binary-expr (third expr)))]
    [(> (length expr) 3)
     (to-binary-expr (append (list (first expr) (list (first expr) (second expr) (third expr))) (list-tail expr 3)))]
    [else expr]))

(define (derivative-upd2 expr var)
  (derivative-upd1 (to-binary-expr expr) var))

(define (simplify-upd2 expr)
  (simplify-upd1 (to-binary-expr expr)))

; -----------------------Exercise 1.8-----------------------

; returns a (sorted) list of
; distinct variables used in a given expression
; Example:
; (variables-of '(+ 1 x y (* x y z))) -> '(x y z)
(define (variables-of-helper variables expr)
  (cond
    [(empty? expr) variables]
    [else
     (if (list? (first expr))
      (variables-of-helper (variables-of-helper variables (first expr)) (rest expr))
      (if (variable? (first expr))
          (if (member (first expr) variables)
              (variables-of-helper variables (rest expr))
              (variables-of-helper (cons (first expr) variables) (rest expr)))
          (variables-of-helper variables (rest expr))))]))

(define (variables-of expr)
  (sort (variables-of-helper empty expr) symbol<?))

; -----------------------Exercise 1.9-----------------------

; tail recusion
(define (gradient-helper vector expr variables)
  (cond
    [(empty? variables) (reverse vector)]
    [else (gradient-helper (cons (simplify-upd2 (derivative-upd2 expr (first variables))) vector)
                           expr
                           (rest variables))]))

; returns a gradient of a multivariable expression
; Example:
; (gradient '(+ 1 x y (* x y z)) '(x y z)) ->
; '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))
(define (gradient expr variables)
  (gradient-helper empty expr variables))


