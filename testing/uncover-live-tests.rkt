#!/usr/bin/env racket
#lang racket
(require rackunit) ; for check-?? funcs
(require racket/exn) ; for exn->string
(require graph)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/uncover-live.rkt")

; **************************************************
; TEST CASES
; **************************************************


; ==================================================
; TEST get-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0 '(addq (int 7) (var y)))
(define expect0 (set 'y))
(check-equal? (get-vars given0) expect0)
; test 2 vars in addq
(define given1 '(addq (var x) (var y)))
(define expect1 (set 'x 'y))
(check-equal? (get-vars given1) expect1)
; test 2 vars in movq
(define given2 '(movq (var x) (var y)))
(define expect2 (set 'x 'y))
(check-equal? (get-vars given2) expect2)
; test 1 var in second arg position in any? binary instruction
(define given3 '(movq (int 10) (var y)))
(define expect3 (set 'y))
(check-equal? (get-vars given3) expect3)
; test 1 var in first arg position in any? binary instruction
(define given4 '(movq (var y) (reg rax)))
(define expect4 (set 'y))
(check-equal? (get-vars given4) expect4)
; test 0 var anywhere in any binary instruction
(define given5 '(movq (int 10) (reg rax)))
(define expect5 (set))
(check-equal? (get-vars given5) expect5)
; test 1 var in any unary instruction
(define given6 '(negq (var t)))
(define expect6 (set 't))
(check-equal? (get-vars given6) expect6)
; test 0 var in any unary instruction
(define given7 '(jmp conclusion))
(define expect7 (set))
(check-equal? (get-vars given7) expect7)
; test bad instr
(define given8 '((int 10) (var y) movq))
(check-fail-with-name 'get-vars get-vars given8)

; ==================================================
; TEST get-read-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0-read '(addq (int 7) (var y)))
(define expect0-read (set 'y))
(check-equal? (get-read-vars given0-read) expect0-read)
; test 2 vars in addq
(define given1-read '(addq (var x) (var y)))
(define expect1-read (set 'x 'y))
(check-equal? (get-read-vars given1-read) expect1-read)
; test 2 vars in movq
(define given2-read '(movq (var x) (var y)))
(define expect2-read (set 'x))
(check-equal? (get-read-vars given2-read) expect2-read)
; test 1 var in second arg position in any? binary instruction
(define given3-read '(movq (int 10) (var y)))
(define expect3-read (set))
(check-equal? (get-read-vars given3-read) expect3-read)
; test 1 var in first arg position in any? binary instruction
(define given4-read '(movq (var y) (reg rax)))
(define expect4-read (set 'y))
(check-equal? (get-read-vars given4-read) expect4-read)
; test 0 var anywhere in any binary instruction
(define given5-read '(movq (int 10) (reg rax)))
(define expect5-read (set))
(check-equal? (get-read-vars given5-read) expect5-read)
; test 1 var in negq
(define given6-read '(negq (var t)))
(define expect6-read (set 't))
(check-equal? (get-read-vars given6-read) expect6-read)
; test 0 var in any unary instruction
(define given7-read '(jmp conclusion))
(define expect7-read (set))
(check-equal? (get-read-vars given7-read) expect7-read)
; test bad instr
(define given8-read '((int 10) (var y) movq))
(check-fail-with-name 'get-read-vars get-read-vars given8-read)

; ==================================================
; TEST get-write-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0-write '(addq (int 7) (var y)))
(define expect0-write (set 'y))
(check-equal? (get-write-vars given0-write) expect0-write)
; test 2 vars in addq
(define given1-write '(addq (var x) (var y)))
(define expect1-write (set 'y))
(check-equal? (get-write-vars given1-write) expect1-write)
; test 2 vars in movq
(define given2-write '(movq (var x) (var y)))
(define expect2-write (set 'y))
(check-equal? (get-write-vars given2-write) expect2-write)
; test 1 var in second arg position in any? binary instruction
(define given3-write '(movq (int 10) (var y)))
(define expect3-write (set 'y))
(check-equal? (get-write-vars given3-write) expect3-write)
; test 1 var in first arg position in any? binary instruction
(define given4-write '(movq (var y) (reg rax)))
(define expect4-write (set))
(check-equal? (get-write-vars given4-write) expect4-write)
; test 0 var anywhere in any binary instruction
(define given5-write '(movq (int 10) (reg rax)))
(define expect5-write (set))
(check-equal? (get-write-vars given5-write) expect5-write)
; test 1 var in negq
(define given6-write '(negq (var t)))
(define expect6-write (set 't))
(check-equal? (get-write-vars given6-write) expect6-write)
; test 0 var in any unary instruction
(define given7-write '(jmp conclusion))
(define expect7-write (set))
(check-equal? (get-write-vars given7-write) expect7-write)
; test bad instr
(define given8-write '((int 10) (var y) movq))
(check-fail-with-name 'get-write-vars get-write-vars given8-write)

; ==================================================
; TEST get-live-after-sets
; ==================================================
; ALSO USED FOR test uncover-live see `given1-uncover`
(define given1-glas '(                           ; 0;     {  }
                      (label start)              ; 1;     {  }
                      (movq (int 1) (var v))     ; 2;     { v }
                      (movq (int 46) (var w))    ; 3;     { v,w }
                      (movq (var v) (var x))     ; 4;     { w,x }
                      (addq (int 7) (var x))     ; 5;     { w,x }
                      (movq (var x) (var y))     ; 6;     { w,x,y }
                      (addq (int 4) (var y))     ; 7;     { w,x,y }
                      (movq (var x) (var z))     ; 8;     { w,y,z }
                      (addq (var w) (var z))     ; 9;     { y,z }
                      (movq (var y) (var t.1))   ; 10;    { t.1, z }
                      (negq (var t.1))           ; 11;    { t.1, z }
                      (movq (var z) (reg rax))   ; 12;    { t.1 }
                      (addq (var t.1) (reg rax)) ; 13;    {  }
                      (jmp conclusion)))         ; 14;    init = { }; (init-Writes) U Reads ??

; ALSO USED FOR test uncover-live see `expect1-uncover`
(define expect1-glas (list
                      (set)          ; 0
                      (set)          ; 1
                      (set 'v)       ; 2
                      (set 'v 'w)    ; 3
                      (set 'w 'x)    ; 4
                      (set 'w 'x)    ; 5
                      (set 'w 'x 'y) ; 6
                      (set 'w 'x 'y) ; 7
                      (set 'w 'y 'z) ; 8
                      (set 'y 'z)    ; 9
                      (set 'z 't.1)  ; 10
                      (set 'z 't.1)  ; 11
                      (set 't.1)     ; 12
                      (set)          ; 13
                      (set)))        ; 14

(check-equal? (get-live-after-sets given1-glas (set)) expect1-glas)


; ==================================================
; TEST get-LAS-from-blocks
; ==================================================
(define given-blocks
  '((block () (label block177) (movq (int 2) (var x)) (jmp block176))
    (block () (label block178) (movq (int 3) (var x)) (jmp block176))
    (block () (label start) (cmpq (int 1) (int 1)) (jmp-if e block177) (jmp block178))
    (block () (label block176) (movq (var x) (reg rax)) (jmp conclusion))))

(define given-hash (make-hash
                    `((block177 ,@(first given-blocks))
                      (block178 ,@(second given-blocks))
                      (start ,@(third given-blocks))
                      (block176 ,@(fourth given-blocks)))))

(define dag_adj_matrix_of_given_blocks
  '((block177 block176)
    (block178 block176)
    (start block177 block178)
    (block176 conclusion)))
(define given-g (unweighted-graph/adj dag_adj_matrix_of_given_blocks))

(define expect-blocks
  `((block
     ; init set of block176 is (set 'x)
     ; Live-After-Set of the "label" instruction is (set 'x)
     ; after that nothing is alive because it will retq
     (,(set 'x),(set 'x) ,(set)                   ,(set))
     (label block176)     (movq (var x) (reg rax)) (jmp conclusion))
    (block
     (,(set),(set)    ,(set 'x)              ,(set 'x))
     (label block178)  (movq (int 3) (var x)) (jmp block176))
    (block
     (,(set),(set)    ,(set 'x)              ,(set 'x))
     (label block177)  (movq (int 2) (var x)) (jmp block176))
    (block
     (,(set),(set) ,(set)                  ,(set)               ,(set))
     (label start)  (cmpq (int 1) (int 1))  (jmp-if e block177)  (jmp block178))))

(check-equal? expect-blocks (get-LAS-from-blocks given-hash given-g))

; ==================================================
; TEST uncover-live
; ==================================================

; NOTE: the ,@ syntax will splice the elements of list into a "quasiquoted list"
; https://docs.racket-lang.org/reference/reader.html?q=%40#%28part._parse-quote%29
(define given1-uncover `(program () ((block () ,@given1-glas))))
(define expect1-uncover `(program () ((block ,expect1-glas ,@given1-glas))))
(check-equal? (uncover-live given1-uncover) expect1-uncover)

(define given-p `(program () ,given-blocks))
(define expect-p `(program () ,expect-blocks))

(check-equal? (uncover-live given-p) expect-p)

(define given2-blocks

  
  ' (
    ; the start block initializes vars a, b, and c
    (block ()                              ; {     }
              (label start)                ; {     }
              (movq (int 1) (var a))       ; {  a  }
              (movq (int 2) (var b))       ; { b,a }
              (movq (int 3) (var c))       ; {b,a,c}
              (cmpq (int 1) (int 1))       ; {b,a,c}
              (jmp-if e block177)          ; {b,a,c}
              (jmp block178))              ; init = {  a  } U { b,c }

    ; a and d are alive in block 177
    (block ()                              ; {  a  }
              (label block177)             ; {  a  }
              (movq (int 4) (var d))       ; { a,d }
              (addq (var d) (var a))       ; {     }
              (jmp block176))              ; init = {     }

    ; b and c are both alive in block 178
    (block ()                              ; { b,c }
              (label block178)             ; { b,c }
              (addq (var b) (var c))       ; {     }
              (jmp block176))              ; init = {     }

    ; no variables are alive in block 176  
    (block ()                              ; {     }
              (label block176)             ; {     }
              (movq (int 0) (reg rax))     ; {     }
              (jmp conclusion))))          ; init = {     }
                                 

(define expect2-blocks
  `((block
     (,(set),(set)       ,(set)                       ,(set))
     (label block176)     (movq (int 0) (reg rax))     (jmp conclusion))
    (block
     (,(set 'b 'c),(set 'b 'c)   ,(set)                   ,(set))
     (label block178)             (addq (var b) (var c))   (jmp block176))
    (block
     (,(set 'a),(set 'a)   ,(set 'a 'd)               ,(set)                    ,(set))
     (label block177)       (movq (int 4) (var d))     (addq (var d) (var a))    (jmp block176))
    (block
     (,(set),(set ) ,(set 'a)               ,(set 'b 'a)            ,(set 'b 'a 'c)          ,(set 'b 'a 'c)          ,(set 'b 'a 'c)       ,(set 'a 'b 'c))
     (label start)   (movq (int 1) (var a))  (movq (int 2) (var b))  (movq (int 3) (var c))   (cmpq (int 1) (int 1))   (jmp-if e block177)   (jmp block178))))

(check-equal? (uncover-live `(program () ,given2-blocks)) `(program () ,expect2-blocks))

(displayln "uncover-live tests finished")
