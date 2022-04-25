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

;Define Memory, Register Indices, CPU (PC, Registers, Stack)
(define-values (x0 zero) (values 0 0))
(define-values (x1 ra) (values 1 1))
(define-values (x2 sp) (values 2 2))
(define-values (x3 gp) (values 3 3))
(define-values (x4 tp) (values 4 4))
(define-values (x5 t0) (values 4 4))
(define-values (x6 t1) (values 4 4))
(define-values (x7 t2) (values 4 4))
(define-values (x8 s0 fp) (values 8 8 8))
(define-values (x9 s1) (values 4 4))
(define-values (x10 a0) (values 4 4))
(define-values (x11 a1) (values 4 4))
(define-values (x12 a2) (values 4 4))
(define-values (x13 a3) (values 4 4))
(define-values (x14 a4) (values 4 4))
(define-values (x15 a5) (values 4 4))
(define-values (x16 a6) (values 4 4))
(define-values (x17 a7) (values 4 4))
(define-values (x18 s2) (values 4 4))
(define-values (x19 s3) (values 4 4))
(define-values (x20 s4) (values 4 4))
(define-values (x21 s5) (values 4 4))
(define-values (x22 s6) (values 4 4))
(define-values (x23 s7) (values 4 4))
(define-values (x24 s8) (values 4 4))
(define-values (x25 s9) (values 4 4))
(define-values (x26 s10) (values 4 4))
(define-values (x27 s11) (values 4 4))
(define-values (x28 t3) (values 4 4))
(define-values (x29 t4) (values 4 4))
(define-values (x30 t5) (values 4 4))
(define-values (x31 t6) (values 4 4))

(struct cpu (pc registers stack) #:transparent)
(struct instruction ())

;RISC-V instructions
(struct op-add (rd rs1 rs2) #:super struct:instruction)
(struct op-sub (rd rs1 rs2) #:super struct:instruction)
(struct op-xor (rd rs1 rs2) #:super struct:instruction)
(struct op-or (rd rs1 rs2) #:super struct:instruction)
(struct op-and (rd rs1 rs2) #:super struct:instruction)
(struct op-sll (rd rs1 rs2) #:super struct:instruction)
(struct op-srl (rd rs1 rs2) #:super struct:instruction)
(struct op-sra (rd rs1 rs2) #:super struct:instruction)
(struct op-slt (rd rs1 rs2) #:super struct:instruction)
(struct op-sltu (rd rs1 rs2) #:super struct:instruction)
(struct op-addi (rd rs1 imm) #:super struct:instruction)
(struct op-xori (rd rs1 imm) #:super struct:instruction)
(struct op-ori (rd rs1 imm) #:super struct:instruction)
(struct op-andi (rd rs1 imm) #:super struct:instruction)
(struct op-slli (rd rs1 imm) #:super struct:instruction)
(struct op-srli (rd rs1 imm) #:super struct:instruction)
(struct op-srai (rd rs1 imm) #:super struct:instruction)
(struct op-slti (rd rs1 imm) #:super struct:instruction)
(struct op-sltiu (rd rs1 imm) #:super struct:instruction)
(struct op-lw (rd imm rs1 ) #:super struct:instruction)
(struct op-sw (rs1 imm rs2 ) #:super struct:instruction)
;branching and jumps
(struct op-lui (rd imm ) #:super struct:instruction)
;auipc

;Replaced instructions
;myinstructions
;jumps and branching
;auipc

;========================= Auxiliary Methods
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

(define (increment-register rd val mem-state)
  (write-register rd (bvadd (read-register rd mem-state) val) mem-state))

(define (increment-pc mem-state)
  (cpu (+ 1 (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

(define (convert-sp-index rd offset mem-state)
 (- (-  (bitvector->integer (bvneg (read-register rd mem-state))) offset) 1))

;========================= RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state)))))

(define (sub rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvsub (read-register rs1 mem-state) (read-register rs2 mem-state)))))

(define (xor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) (read-register rs2 mem-state)))))

(define (or rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) (read-register rs2 mem-state)))))

(define (and rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvand (read-register rs1 mem-state) (read-register rs2 mem-state)))))

(define (sll rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))))))

(define (srl rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))))))

(define (sra rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))))))

(define (slt rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) (read-register rs2 mem-state)) (int32 1) (int32 0)))))

(define (sltu rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) (read-register rs2 mem-state)) (int32 1) (int32 0)))))

;============== I-Type
(define (addi rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) imm) mem-state)))

(define (xori rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) imm))))

(define (ori rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) imm))))

(define (andi rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvand (read-register rs1 mem-state) imm))))

(define (slli rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand imm (int32 31))))))

(define (srli rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand imm (int32 31))))))

(define (srai rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand imm (int32 31))))))

(define (slti rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) imm) (int32 1) (int32 0)))))

(define (sltiu rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) imm) (int32 1) (int32 0)))))

;============== Memory
(define (lw rd imm rs1 mem-state)
  (let ([ind (convert-sp-index rs1 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (bitvector->integer (bvneg (read-register x2 mem-state)))))
    (increment-pc (write-register rd (list-ref (cpu-stack mem-state) ind) mem-state))))

;syntax: sw x3, 0(sp) --> sw x3, 0, sp
(define (sw rs1 imm rs2 mem-state)
  (let ([ind (convert-sp-index rs2 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (bitvector->integer (bvneg (read-register x2 mem-state)))))
   (increment-pc (cpu
   (cpu-pc mem-state)
   (cpu-registers mem-state)
   (list-set (cpu-stack mem-state) ind (read-register rs1 mem-state))))))

;============== Branching and Jumps

;========================= Replaced Instructions
;============== R-Type
;============== I-Type
;============== Memory
;============== Branching and Jumps


;define testprogram e.g. (myadd x10 x7 x3 4 register stack)
;define stack, registers etc
;loop that executes program
;(function which takes program as parameter and returns registers etc? for verification)
;#########################################

(define (execute-instruction instruction mem-state)
  (match instruction
    [(op-add rd rs1 rs2) (add rd rs1 rs2 mem-state)]
    [(op-sub rd rs1 rs2) (sub rd rs1 rs2 mem-state)]
    [(op-xor rd rs1 rs2) (xor rd rs1 rs2 mem-state)]
    [(op-or rd rs1 rs2) (or rd rs1 rs2 mem-state)]
    [(op-and rd rs1 rs2) (and rd rs1 rs2 mem-state)]
    [(op-sll rd rs1 rs2) (sll rd rs1 rs2 mem-state)]
    [(op-srl rd rs1 rs2) (srl rd rs1 rs2 mem-state)]
    [(op-sra rd rs1 rs2) (sra rd rs1 rs2 mem-state)]
    [(op-slt rd rs1 rs2) (slt rd rs1 rs2 mem-state)]
    [(op-sltu rd rs1 rs2) (sltu rd rs1 rs2 mem-state)]
    [(op-addi rd rs1 imm) (addi rd rs1 imm mem-state)]
    [(op-xori rd rs1 imm) (xori rd rs1 imm mem-state)]
    [(op-ori rd rs1 imm) (ori rd rs1 imm mem-state)]
    [(op-andi rd rs1 imm) (andi rd rs1 imm mem-state)]
    [(op-slli rd rs1 imm) (slli rd rs1 imm mem-state)]
    [(op-srli rd rs1 imm) (srli rd rs1 imm mem-state)]
    [(op-srai rd rs1 imm) (srai rd rs1 imm mem-state)]
    [(op-slti rd rs1 imm) (slti rd rs1 imm mem-state)]
    [(op-sltiu rd rs1 imm) (sltiu rd rs1 imm mem-state)]
    [(op-lw rd imm rs1) (lw rd imm rs1 mem-state)]
    [(op-sw rs1 imm rd) (sw rs1 imm rd mem-state)]
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