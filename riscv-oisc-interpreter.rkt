#lang rosette
(require threading)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))

; int32 takes as input an integer literal and returns the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; Parameter that controls the number of unrollings, for the main execute method (to prevent infinite runs), has to be adjusted
(define fuel (make-parameter 30))

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
(define-values (x5 t0) (values 5 5))
(define-values (x6 t1) (values 6 6))
(define-values (x7 t2) (values 7 7))
(define-values (x8 s0 fp) (values 8 8 8))
(define-values (x9 s1) (values 9 9))
(define-values (x10 a0) (values 10 10))
(define-values (x11 a1) (values 11 11))
(define-values (x12 a2) (values 12 12))
(define-values (x13 a3) (values 13 13))
(define-values (x14 a4) (values 14 14))
(define-values (x15 a5) (values 15 15))
(define-values (x16 a6) (values 16 16))
(define-values (x17 a7) (values 17 17))
(define-values (x18 s2) (values 18 18))
(define-values (x19 s3) (values 19 19))
(define-values (x20 s4) (values 20 20))
(define-values (x21 s5) (values 21 21))
(define-values (x22 s6) (values 22 22))
(define-values (x23 s7) (values 23 23))
(define-values (x24 s8) (values 24 24))
(define-values (x25 s9) (values 25 25))
(define-values (x26 s10) (values 26 26))
(define-values (x27 s11) (values 27 27))
(define-values (x28 t3) (values 28 28))
(define-values (x29 t4) (values 29 29))
(define-values (x30 t5) (values 30 30))
(define-values (x31 t6) (values 31 31))

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
(struct op-lw (rd imm rs1) #:super struct:instruction)
(struct op-sw (rs1 imm rs2) #:super struct:instruction)
;branching and jumps
(struct op-lui (rd imm) #:super struct:instruction)
;auipc

;Replaced instructions
(struct op-myor (rd rs1 rs2) #:super struct:instruction)
;
(struct op-myaddi (rd rs1 imm) #:super struct:instruction)
;myinstructions
;jumps and branching
;auipc

;========================= Auxiliary Methods
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

;incrementes by 1 to use it as index for the program list, and making it comparable
(define (increment-pc mem-state)
  (cpu (+ 1 (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

(define (convert-sp-index rd offset mem-state)
  (- (- (- (bitvector->integer (read-register rd mem-state))) offset) 1))

(define (set-pc pc mem-state)
  (cpu pc (cpu-registers mem-state) (cpu-stack mem-state)))

;========================= RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (sub rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvsub (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

;added r for risc-v to the name, rosette already has xor/or/and defined as functions
(define (rxor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state) ))

(define (ror rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (rand rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvand (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (sll rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))) mem-state)))

(define (srl rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))) mem-state)))

(define (sra rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (int32 31))) mem-state)))

(define (slt rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) (read-register rs2 mem-state)) (int32 1) (int32 0)) mem-state)))

(define (sltu rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) (read-register rs2 mem-state)) (int32 1) (int32 0)) mem-state)))

;============== I-Type
(define (addi rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) imm) mem-state)))

(define (xori rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) imm) mem-state)))

(define (ori rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) imm) mem-state)))

(define (andi rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvand (read-register rs1 mem-state) imm) mem-state)))

(define (slli rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand imm (int32 31))) mem-state)))

(define (srli rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand imm (int32 31))) mem-state)))

(define (srai rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand imm (int32 31))) mem-state)))

(define (slti rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) imm) (int32 1) (int32 0)) mem-state)))

(define (sltiu rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) imm) (int32 1) (int32 0)) mem-state)))

;============== Memory
(define (lw rd imm rs1 mem-state)
  (let ([ind (convert-sp-index rs1 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (- (bitvector->integer (read-register x2 mem-state)))))
    (increment-pc (write-register rd (list-ref (cpu-stack mem-state) ind) mem-state))))

;syntax: sw x3, 0(sp) --> sw x3, 0, sp
(define (sw rs1 imm rs2 mem-state)
  (let ([ind (convert-sp-index rs2 imm mem-state)])
    (assert (< ind (length (cpu-stack mem-state))))
    (assert (< ind (- (bitvector->integer (read-register x2 mem-state)))))
    (increment-pc (cpu
    (cpu-pc mem-state)
    (cpu-registers mem-state)
    (list-set (cpu-stack mem-state) ind (read-register rs1 mem-state))))))

;============== B, J, U - Types
(define (lui rd imm mem-state)
  (increment-pc (write-register rd (bvshl imm (int32 12)) mem-state)))

;========================= Replaced Instructions
;============== R-Type
(define (myor rd rs1 rs2 mem-state)
  (increment-pc mem-state))

;============== I-Type
(define (myaddi rd rs1 imm mem-state)
  (~>> mem-state
  (addi sp sp (int32 -5))
  (sw t0 0 sp)
  (sw t1 1 sp)
  (sw t2 2 sp)
  (sw rs1 3 sp)
  (lw t1 3 sp)
  (addi t0 x0 imm)
  (sub t0 x0 t0)
  (sub t2 t1 t0)
  (sw t2 4 sp)
  (lw t2 2 sp)
  (lw t1 1 sp)
  (lw t0 0 sp)
  (lw rd 4 sp)
  (addi sp sp (int32 5))
  (set-pc (cpu-pc mem-state))
  (increment-pc)))

;============== Memory
;============== B, J, U - Types

;========================= Main Functions

(define (execute-instruction instruction mem-state)
  (match instruction
    [(op-add rd rs1 rs2) (add rd rs1 rs2 mem-state)]
    [(op-sub rd rs1 rs2) (sub rd rs1 rs2 mem-state)]
    [(op-xor rd rs1 rs2) (rxor rd rs1 rs2 mem-state)]
    [(op-or rd rs1 rs2) (ror rd rs1 rs2 mem-state)]
    [(op-and rd rs1 rs2) (rand rd rs1 rs2 mem-state)]
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
    ;B, J , U
    [(op-lui rd imm) (lui rd imm mem-state)]
    ;replaced instructions
    [(op-myor rd rs1 rs2) (myor rd rs1 rs2 mem-state)]
    ;
    [(op-myaddi rd rs1 imm) (myaddi rd rs1 imm mem-state)]
    ))

(define-bounded (execute-program instructions mem-state)
  (let ([pc (cpu-pc mem-state)] [len (length instructions)])
    (cond
    [(< pc len)
     (execute-program instructions (execute-instruction (list-ref instructions pc) mem-state))]
    [else mem-state])))


;######################################### Test-Programs
(define test-cpu (cpu 0
                  (list (int32 0) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))
                  (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))))

(define-symbolic test-pc integer?)
(define-symbolic test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31 int32?)
(define-symbolic test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31 int32?)

(define test-mem-state (cpu
                        test-pc
                        (list (int32 0) test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31)
                        (list test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31)))

(define-symbolic test2-pc integer?)
(define-symbolic test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31 int32?)
(define-symbolic test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31 int32?)

(define test2-mem-state (cpu
                        test2-pc
                        (list (int32 0) test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31)
                        (list test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31)))

;(verify
;   (begin
;     (assume (eq? (list-ref (cpu-registers test-cpu) 0) 0))
;     (assume (< 31 (length (cpu-registers test-cpu))))
;     (assert (eq? (read-register x0 test-cpu) 0))))

;; ;Test-Program 1
;; (define program1 (list (op-addi x1 x1 (int32 3)) (op-addi sp sp (int32 -1)) (op-sw x1 0 sp) (op-lw x4 0 sp) (op-addi sp sp (int32 1))))
;; (define memory1 (execute-program program1 test-cpu))
;; ;memory1

;; (define program2 (list (op-myaddi x1 x1 (int32 3)) (op-myaddi sp sp (int32 -1)) (op-sw x1 0 sp) (op-lw x4 0 sp) (op-myaddi sp sp (int32 1))))
;; (define memory2 (execute-program program2 test-cpu))
;memory2

(define (eq-mem-state memory1 memory2)
  (and
   (equal? (cpu-pc memory1) (cpu-pc memory2))
   (equal? (cpu-registers memory1) (cpu-registers memory2))
   (equal?
    (take (cpu-stack memory1) (- (bitvector->integer (read-register sp memory1))))
    (take (cpu-stack memory2) (- (bitvector->integer (read-register sp memory2))))
   )))
;comparing stack only works if program doesn't allocate memory that it doesn't use

;verify that if mem-states are equal, executing  instructions add and myadd produce still equivalent states
; parameters for the verifications
(define-symbolic imm int32?)
(define-symbolic rd rs1 integer?)

(define (valid-src-reg reg)
  (and (>= reg 0) (not (= reg sp)) (<= reg 31)))

(define (valid-dest-reg reg)
  (and (>= reg 1) (<= reg 31)))

(verify (begin (assume (and
                    (< (- (bitvector->integer (read-register sp test-mem-state))) (- (length (cpu-stack test-mem-state)) 20))
                    (<= (bitvector->integer (read-register sp test-mem-state)) 0)
                    (valid-src-reg rs1)
                    (valid-dest-reg rd)
                    (not (= rd sp))      ;exclude sp because of the comment above
                    (eq-mem-state test-mem-state test2-mem-state)))
               (assert (eq-mem-state (addi rd rs1 imm test-mem-state) (myaddi rd rs1 imm test2-mem-state)))))

;(eq-mem-state memory1 memory2)
;cex
;(evaluate (list test-mem-state test2-mem-state) cex)
;(evaluate (list rd rs1 imm (addi rd rs1 imm test-mem-state) (myaddi rd rs1 imm test2-mem-state)) cex)
;(display "mem2\n")
;; (evaluate (~>> memory2
;;  (addi sp sp (int32 -5))
;;  (sw t0 0 sp)
;;  (sw t1 1 sp)
;;  (sw t2 2 sp)
;;  (sw rs1 3 sp)
;;  (lw t1 3 sp)
;;  (addi t0 x0 imm)
;;  (sub t0 x0 t0)
;;  (sub t2 t1 t0)
;;  (sw t2 4 sp)
;;  (lw t2 2 sp)
;;  (lw t1 1 sp)
;;  (lw t0 0 sp)
;;  (lw x1 4 sp)
;;  (addi sp sp (int32 5))
;;  (set-pc (cpu-pc memory2))
;;  (increment-pc)) cex)