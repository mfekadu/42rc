#!/usr/bin/env racket
#lang racket

(provide typecheck-exp)
(provide typecheck)

(define (comparison-op? x)
    (set-member? (set '< '> '<= '>=) x))

(define (int-op? x)
    (set-member? (set '+ '-) x))

(define (logical-op? x)
    (set-member? (set 'and 'or) x))

; typecheck-exp returns the type of an expression
; it will error if the expression mismatches types
(define (typecheck-exp env e)
  (match e
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol? x) (dict-ref env x)]
    ['(read) 'Integer]
    [`(let ([,var ,val]) ,body)

      ; TODO does something more complicated need to happen here with variable shadowing?
      ; TODO can/should we just call uniquify first?
     (define var-type (typecheck-exp env val))
     (define new-env (cons (cons var var-type) env))
     (typecheck-exp new-env body)]
    
    ; separate case for unary negation
    [`(- ,e)
      (define t (typecheck-exp env e))
      (cond
        [(eq? t 'Integer) 'Integer]
        [else (error "typechek: Type mismatch on expr: ~v" e)])]

    [`(not ,e)
      (define t (typecheck-exp env e))
      (cond
        [(eq? t 'Boolean) 'Boolean]
        [else (error "typechek: Type mismatch on expr: ~v" e)])]

    [`(,(? int-op? _) ,e1 ,e2)
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))
      (cond
        [(and (eq? t1 'Integer) (eq? t2 'Integer)) 'Integer]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])]

    [`(eq? ,e1 ,e2)
      ; need to call typecheck on e1 and e2 to check for errors
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))

      ; this isn't explicitly necessary (yet), but in case we had other types (like Strings)
      ; I think eq? should only operate on Bools and Ints
      (cond
        [(or
           (and (eq? t1 'Boolean) (eq? t2 'Boolean))
           (and (eq? t1 'Integer) (eq? t2 'Integer)))
         'Boolean]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])]

    ; handle boolean ops
    [`(,(? comparison-op? op) ,e1 ,e2)
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))
      (cond
        ; TODO is this the correct way to compare symbols?
        ; return a boolean
        [(and (eq? t1 'Integer) (eq? t2 'Integer)) 'Boolean]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])] 

    [`(,(? logical-op? op) ,e1 ,e2)
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))
      (cond
        ; TODO is this the correct way to compare symbols?
        ; return a boolean
        [(and (eq? t1 'Boolean) (eq? t2 'Boolean)) 'Boolean]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])]

    [`(if ,cmp ,thn ,els)
      (define cmp-type (typecheck-exp env cmp))
      (define thn-type (typecheck-exp env thn))
      (define els-type (typecheck-exp env els))
      (cond
        ; TODO is this the correct way to compare symbols?
        ; return a boolean
        [(and
           (eq? cmp-type 'Boolean)
           (eq? thn-type els-type))
         thn-type]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])]

    [_ (error 'typecheck-exp "Unrecognized expr ~v" e)]))




; top level typecheck function
; calls typecheck-exp on the body of the program
; returns its input if there was no error
(define (typecheck program)
  (match program
    [`(program ,info ,body)

     ; find the type of the body, but don't do anything with it
     ; it will error if there's a type mismatch somewhere 
     (define type (typecheck-exp '() body))

     ; return the input program if no type errors
     `(program ,info ,body)]

    [_ (error 'typecheck "Malformed program input to typecheck")]))
