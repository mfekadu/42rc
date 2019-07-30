#!/usr/bin/racket
#lang racket
(require rackunit)

(define (make-let var val body)
  (list 'let (list [list var val]) body))


(define not-empty? (λ (l) (not (empty? l))))
;test not empty
(check-false (not-empty? '()))
(check-true (not-empty? '(1 2)))

; Given an expr in R1, return an expr in R1 without any complex subexpressions
(define (rco-exp exprs) ; returns expr
  (match exprs
    ; handle base cases
    [(or (? symbol?) (? integer?) '(read)) exprs]
    ; ??? in rust we add this binding to the alist but maybe that's not right...
    [(list 'let (list [list var val]) body)
     (displayln (list var val body)) 
     (error "rco-exp let case")]
    ; this case should call rco-arg on each of the args
    ; then build a new expr with bindings from the rco-arg calls
    [(list op args ...)
     (displayln (list "match-op-exprs" exprs))
     (define-values [syms alists]
       (for/lists (l1 l2) 
                ([e exprs])
       (rco-arg e)))
     (displayln (list "rco-exp syms " syms))
     (displayln (list "rco-exp alists " alists))
     ; loop over alists

     ; generate the final expr by
     (for/list ([binds alists] ; loop over both the binds
                [sym syms])    ; and the corresponding syms
       (match binds
         ; match anything complex
         [(list var val) (make-let var val syms)]
         ; match the op symbol for e.g.
         [_ sym]))]))

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
;(check-equal? (rco-exp (list '+ 2 2)) '(+ 2 2))
;(check-equal? (rco-exp '(let ([x 2]) x)) '(let ([x 2]) x))
;(check-equal? (rco-exp (list 'read)) '(read))
;

(displayln '(rco-exp '(+ (- 3) (- 4))))
(displayln (rco-exp '(+ (- 3) (- 4))))
 

(displayln "tests finished")
