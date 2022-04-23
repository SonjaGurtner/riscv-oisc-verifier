#lang rosette
(require rackunit)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))

; int32 takes as input an integer literal and returns the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; define memory
;(struct register ())
;(struct x0 () #:super struct:register)
;(struct registers (x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) #:transparent)
(define-values (x0 zero) (values 0 0))
(define-values (x1 ra) (values 1 1))
(define-values (x2 sp) (values 2 2))
(define-values (x3 gp) (values 3 3))
;TODO registers
(struct cpu (pc registers stack) #:transparent)

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
  (cpu (cpu-pc mem-state) (list-set (cpu-registers mem-state) rd val) (cpu-stack mem-state))) ;(registers-x1 (cpu-registers cpu))

;====================== RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state))))

;============== I-Type
(define (addi rd rs1 imm mem-state)
  (write-register rd (bvadd (read-register rs1 mem-state) imm) mem-state))

(define (lw rd imm mem-state)
  ;(registers-rd (cpu-registers cpu))
  ;set rd to value from stack at imm(sp)
  mem-state)

;============== S-Type
(define (sw rs1 imm mem-state)
  ;store rs1 to stack at imm(sp)
  mem-state)

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
;#########################################
;Tests
(define-symbolic b boolean?)
(define test-cpu
  (if b
      (cpu 0 (list (int32 0) (int32 1) (int32 1) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0)) 0)
      (cpu 1 (list (int32 0) (int32 1) (int32 1) (int32 1) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0)) 1)))
(verify
   (begin
     (assume (eq? (list-ref (cpu-registers test-cpu) 0) 0))
     (assume (< 31 (length (cpu-registers test-cpu))))
     (assert (eq? (read-register x0 test-cpu) 0))))

(addi x1 x2 (int32 6) test-cpu)
