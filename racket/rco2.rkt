#!/usr/bin/racket
#lang racket
(require rackunit)

; https://docs.racket-lang.org/rackunit/api.html#%28def._%28%28lib._rackunit%2Fmain..rkt%29._check-exn%29%29
(define (check-fail thunk)
  (check-exn exn:fail? thunk))

(define (make-let var val body)
  (list 'let (list [list var val]) body))

(define OPS (hash '+ 1 '- 2 'read 3))
(define (op? x) (hash-has-key? OPS x))
(check-true (op? '+))
(check-false (op? 1))

(define not-empty? (λ (l) (not (empty? l))))
;test not empty
(check-false (not-empty? '()))
(check-true (not-empty? '(1 2)))

; recursively generate bindings
(define (make-nested-lets bindings body)
  (cond [(empty? bindings) body] ; base base case
        [else (match (first bindings)
                [(? empty?)
                 ; at this point bindings is not empty
                 ; but (first bindings) is empty
                 ; (rest bindings) is a perfectly safe call
                 ; due to the nature of pairs in Racket
                 ; i.e. keep recursing because there could be more bindings
                 (make-nested-lets (rest bindings) body)]
                [(list var val)
                 (make-let var val (make-nested-lets (rest bindings) body))]
                [_ (error "unexpected")])]))

; TEST make-nested-lets
(check-equal? (make-nested-lets (list (list 'tmp '(- 2))) 'body) '(let ((tmp (- 2))) body))
(check-equal? (make-nested-lets (list) 'body) 'body)
(check-fail (λ () (make-nested-lets (list 'foo 'bar) 'body)))
(check-equal? (make-nested-lets (list '() '(tmp (- 2))) 'body) '(let [[tmp (- 2)]] body))
; now a legit test case
(check-equal? (make-nested-lets (list '(tmp1 (+ 3 4)) '(tmp2 (- 2))) '(+ tmp1 tmp2))
              '(let [[tmp1 (+ 3 4)]]
                    (let [[tmp2 (- 2)]]
                         (+ tmp1 tmp2))))

; Given an expr in R1, return an expr in R1 without any complex subexpressions
(define (rco-exp exprs) ; returns expr
  (match exprs
    ; handle base cases
    [(or (? symbol?) (? integer?) '(read)) exprs]
    ; ??? in rust we add this binding to the alist but maybe that's not right...
    [(list 'let (list [list var val]) body)
     (error "rco-exp let case")]
    ; this case should call rco-arg on each of the args
    ; then build a new expr with bindings from the rco-arg calls
    [(list (? op? op) args ...)
     (define-values [syms bindings]
       (for/lists (l1 l2) 
                ([e exprs])
       (rco-arg e)))
     ; generate the final expr
     (make-nested-lets bindings syms)]
    [_ "panic!"]))

 ; Given an expr in R1, return a temp symbol name and an alist mapping from the symbol name to an expr in R1
(define (rco-arg exprs) ; returns expr, alist
  (match exprs
    ; handle simple base cases
    [(or (? symbol?) (? integer?) '(read)) (values exprs '())]
    ; TODO let case should bind var to val in an alist and evaluate the body somehow 
    [(list 'let (list [list var val]) body) (error "rco-arg let case")]
    [(list op args ...) (let ([tmp-name (gensym 'tmp)])
                          (values tmp-name
                                  (list tmp-name exprs)))]))

; TEST HELPERS
(define make-list-from-vals (λ (a b) (list a b)))

(define (verify-rco-arg-output-is-empty given)
  (check-match 
    (call-with-values (λ () (rco-arg given)) make-list-from-vals) 
    (list given (? empty?))))

(define (verify-rco-arg-output given expect)
  (check-match 
    (call-with-values (λ () (rco-arg given)) make-list-from-vals) 
    (list (? symbol? s) (list (? symbol? s) expect))))

; TEST CASES
; ATOMS should stay simple rco-exp
(check-equal? (rco-exp 2) 2)
(check-equal? (rco-exp '+) '+)
; ATOMS should stay simple rco-arg
(verify-rco-arg-output-is-empty 2)
(verify-rco-arg-output-is-empty '+)


; OPERATIONS should get simplied by rco-arg
(verify-rco-arg-output '(+ 2 2) '(+ 2 2))

; SIMPLE OPERATIONS should stay simple when called by rco-exp
(check-equal? (rco-exp '(+ 2 2)) '(+ 2 2))
(check-equal? (rco-exp (list '+ 2 2)) '(+ 2 2))
; TODO: handle this...
;(check-equal? (rco-exp '(let ([x 2]) x)) '(let ([x 2]) x))
(check-equal? (rco-exp (list 'read)) '(read))

(displayln "yes")
(rco-exp '(+ 2 (- (+ 3 4))))
;(rco-arg '(let ([x 1]) x))
;
;; BAD exprs rco-exp
;(check-equal? (rco-exp (list 2)) "panic!")
;(check-equal? (rco-exp '(x)) "panic!")
;(check-equal? (rco-exp (list '+)) "panic!")
;(check-equal? (rco-exp #t) "panic!")
;; BAD exprs rco-arg
;(check-equal? (rco-arg #t) "panic!")
;
;; SIMPLE exprs SHOULD STAY SIMPLE
;

(displayln '(rco-exp '(+ (- 3) (- 4))))
(displayln (rco-exp '(+ (- 3) (- 4))))
 

(displayln "tests finished")
