#lang rosette

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))

; int32 takes as input an integer literal and returns the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; Parameter that controls the number of unrollings, for the main execute method (to prevent infinite runs), has to be adjusted
(define fuel (make-parameter 5))

(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Limit of loop runs reached")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

; define memory
;(struct register ())
;(struct x0 () #:super struct:register)
;(struct registers (x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) #:transparent)
(define-values (x0 zero) (values 0 0))
(define-values (x1 ra) (values 1 1))
(define-values (x2 sp) (values 2 2))
(define-values (x3 gp) (values 3 3))
(define-values (x4 tp) (values 4 4))
;TODO registers
(struct cpu (pc registers stack) #:transparent)
(struct instruction ())
;RISC-V instructions
(struct op-addi (rd rs1 imm) #:super struct:instruction)
(struct op-sw (rs1 imm rs2 ) #:super struct:instruction)
(struct op-lw (rd imm rs1 ) #:super struct:instruction)
;Replaced instructions

;====================== Auxiliary Instructions - Memory
(define (modify-stack mem-state len)
  (assume (< len (length (cpu-stack mem-state))))
  cpu)

(define (read-register rd mem-state)
  (assume (< rd 32))
  (assume (< 31 (length (cpu-registers mem-state))))
  (list-ref (cpu-registers mem-state) rd))

(define (write-register rd val mem-state)
  (assume (> rd 0))
  (assume (< rd 32))
  (cpu (cpu-pc mem-state) (list-set (cpu-registers mem-state) rd val) (cpu-stack mem-state)))

(define (increment-pc mem-state)
  (cpu (+ 1 (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

(define (convert-sp-index rd offset mem-state)
 (- (-  (bitvector->integer (bvneg (read-register rd mem-state))) offset) 1))

;====================== RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state)))))

;============== I-Type
(define (addi rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) imm) mem-state)))

(define (lw rd imm rs1 mem-state)
  (let ([ind (convert-sp-index rs1 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (bitvector->integer (bvneg (read-register x2 mem-state)))))
    (increment-pc (write-register rd (list-ref (cpu-stack mem-state) ind) mem-state))))

;============== S-Type
;syntax: sw x3, 0(sp) --> sw x3, 0, sp
(define (sw rs1 imm rs2 mem-state)
  (let ([ind (convert-sp-index rs2 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (bitvector->integer (bvneg (read-register x2 mem-state)))))
   (increment-pc (cpu
   (cpu-pc mem-state)
   (cpu-registers mem-state)
   (list-set (cpu-stack mem-state) ind (read-register rs1 mem-state))))))

;====================== Replaced Instructions
;============== R-Type
(define (myadd rd rs1 rs2 cpu)
   ;(bvadd (registers-x2 (cpu-registers cpu)) (int32 -24)) ;sp -24 -> cpu->registers->x2
   ;store registers and rs1, rs2
   ;sub
   ;sub
   ;restore registers
   ;sp +24
   cpu)

;define testprogram e.g. (myadd x10 x7 x3 4 register stack)
;define stack, registers etc
;loop that executes program
;(function which takes program as parameter and returns registers etc? for verification)
;#########################################

(define (execute-instruction instruction mem-state)
  (match instruction
    [(op-addi rd rs1 imm) (addi rd rs1 imm mem-state)]
    ;[(op-myaddi rd rs1 imm) (myaddi rd rs1 imm mem-state)]
    [(op-sw rs1 imm rd) (sw rs1 imm rd mem-state)]
    [(op-lw rd imm rs1) (lw rd imm rs1 mem-state)]
    ))
;TODO alle instructions

(define-bounded (execute-program instructions mem-state)
  (let ([pc (cpu-pc mem-state)] [len (length instructions)])
    (cond
    [(< pc len)
     (execute-program instructions (execute-instruction (list-ref instructions pc) mem-state))]
    [else mem-state])))


;######################################### Test-Bench
;; (define-symbolic b boolean?)
;; (define test-cpu
;;   (if b
;;       (cpu 0 (list (int32 0) (int32 1) (int32 1) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0)) 0)
;;       (cpu 0 (list (int32 0) (int32 1) (int32 1) (int32 1) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0)) 1)))

;(define-symbolic* x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 int32?)

(define test-cpu (cpu 0
                  (list (int32 0) (int32 1) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))
                   (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))))

(verify
   (begin
     (assume (eq? (list-ref (cpu-registers test-cpu) 0) 0))
     (assume (< 31 (length (cpu-registers test-cpu))))
     (assert (eq? (read-register x0 test-cpu) 0))))

;(addi x1 x2 (int32 6) test-cpu)

;Test-Program 1
(define program (list (op-addi x1 x1 (int32 3)) (op-addi x2 x0 (int32 -1)) (op-sw x1 0 sp) (op-lw x4 0 sp)))
(define memory1 (execute-program program test-cpu))
memory1