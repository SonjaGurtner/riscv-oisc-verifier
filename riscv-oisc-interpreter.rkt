#lang rosette
(require threading)
(provide (all-defined-out))

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
(struct op-myadd (rd rs1 rs2) #:super struct:instruction)
;(struct op-myxor (rd rs1 rs2) #:super struct:instruction)
;(struct op-myor (rd rs1 rs2) #:super struct:instruction)
;(struct op-myand (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysll (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysrl (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysra (rd rs1 rs2) #:super struct:instruction)
;(struct op-myslt (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysltu (rd rs1 rs2) #:super struct:instruction)
(struct op-myaddi (rd rs1 imm) #:super struct:instruction)
;(struct op-myxori (rd rs1 imm) #:super struct:instruction)
;(struct op-myori (rd rs1 imm) #:super struct:instruction)
;(struct op-myandi (rd rs1 imm) #:super struct:instruction)
;(struct op-myslli (rd rs1 imm) #:super struct:instruction)
;(struct op-mysrli (rd rs1 imm) #:super struct:instruction)
;(struct op-mysrai (rd rs1 imm) #:super struct:instruction)
;(struct op-myslti (rd rs1 imm) #:super struct:instruction)
;(struct op-mysltiu (rd rs1 imm) #:super struct:instruction)
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
  (cpu (+ 4 (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

(define (convert-sp-index rd offset mem-state)
  (- (/ (- (- (bitvector->integer (read-register rd mem-state))) offset) 4) 1))

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
(define (myadd rd rs1 rs2 mem-state)
  (~>> mem-state
  (addi sp sp (int32 -24))
  (sw t0 0 sp)
  (sw t1 4 sp)
  (sw t2 8 sp)
  (sw rs1 12 sp)
  (sw rs2 16 sp)
  (lw t1 12 sp)
  (lw t0 16 sp)
  (sub t0 x0 t0)
  (sub t2 t1 t0)
  (sw t2 20 sp)
  (lw t2 8 sp)
  (lw t1 4 sp)
  (lw t0 0 sp)
  (lw rd 20 sp)
  (addi sp sp (int32 24))
  (set-pc (cpu-pc mem-state))
  (increment-pc)))

(define (myor rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)]
        [mem1 (~>> mem-state
              (addi sp sp -44)
              (sw t3 0 sp)
              (sw t4 4 sp)
              (sw t5 8 sp)
              (sw t6 12 sp)      ;msb eliminated rs1
              (sw t0 16 sp)      ;msb eliminated rs2
              (sw t1 20 sp)      ;loop variable i
              (sw t2 24 sp)      ;loop limit 32
              (sw s1 28 sp)
              (sw rs1 32 sp)
              (sw rs2 36 sp)
              (lw t3 32 sp)
              (lw t4 36 sp)
              (addi t0 x0 1)
              (addi t1 x0 1)
              (addi t2 x0 33)
              (sub s1 x0 x0))])
              ;main loop, loops over every bit
       (for ([i (read-register t2)])
         (assert (eq? (+ 1 i) (read-register t1)))
         (slli s1 s1 1)
         (slli t5 t3 1)
         (srli t5 t5 1)
         (slli t6 t4 1)
         (srli t6 t6 1)
         ;do magic
         )

    (increment-pc (set-pc start-pc mem1)))

 
  (sub t5 t3 t5)
  (MYBNE t5 x0 102f)
  (110:)
  (sub t6 t4 t6)
  (MYBNE t6 x0 103f)
  (111:)
  (MYADD t5 t5 t6)
  (blt x0 t5 104f)
  (beq x0 x0 105f)

  102
  (MYADDI t5 x0 1)
  (beq x0 x0 110b)

  103
  (MYADDI t6 x0 1)
  (beq x0 x0 111b)

  104
  (MYADDI s1 s1 1)

  105
  (MYADDI t1 t1 1)
  (MYSLLI_SAFE t3 t3 1)
  (MYSLLI_SAFE t4 t4 1)
  (beq x0 x0 101b)

  107
  (sub rd x0 r2)
  (sub rd x0 rd)
  (beq x0 x0 109f)

  108
  (sub rd x0 r1)
  (sub rd x0 rd)
  (beq x0 x0 109f)

  106
  (sw s1 40 sp)
  (lw s1 28 sp)
  (lw t2 24 sp)
  (lw t1 20 sp)
  (lw t0 16 sp)
  (lw t6 12 sp)
  (lw t5 8 sp)
  (lw t4 4 sp)
  (lw t3 0 sp)
  (lw rd 40 sp)
  (addi sp sp 44)

  (set-pc (cpu-pc mem-state))
  (increment-pc))

;============== I-Type
(define (myaddi rd rs1 imm mem-state)
  (~>> mem-state
  (addi sp sp (int32 -20))
  (sw t0 0 sp)
  (sw t1 4 sp)
  (sw t2 8 sp)
  (sw rs1 12 sp)
  (lw t1 12 sp)
  (addi t0 x0 imm)
  (sub t0 x0 t0)
  (sub t2 t1 t0)
  (sw t2 16 sp)
  (lw t2 8 sp)
  (lw t1 4 sp)
  (lw t0 0 sp)
  (lw rd 16 sp)
  (addi sp sp (int32 20))
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
    [(op-myadd rd rs1 rs2) (myadd rd rs1 rs2 mem-state)]
    ;; [(op-myxor rd rs1 rs2) (myxor rd rs1 rs2 mem-state)]
    ;; [(op-myor rd rs1 rs2) (myor rd rs1 rs2 mem-state)]
    ;; [(op-myand rd rs1 rs2) (myand rd rs1 rs2 mem-state)]
    ;; [(op-mysll rd rs1 rs2) (mysll rd rs1 rs2 mem-state)]
    ;[(op-mysrl rd rs1 rs2) (mysrl rd rs1 rs2 mem-state)]
    ;[(op-mysra rd rs1 rs2) (mysra rd rs1 rs2 mem-state)]
    ;; [(op-myslt rd rs1 rs2) (myslt rd rs1 rs2 mem-state)]
    ;; [(op-mysltu rd rs1 rs2) (mysltu rd rs1 rs2 mem-state)]
    [(op-myaddi rd rs1 imm) (myaddi rd rs1 imm mem-state)]
    ;; [(op-myxori rd rs1 imm) (myxori rd rs1 imm mem-mystate)]
    ;[(op-myori rd rs1 imm) (myori rd rs1 imm mem-mystate)]
    ;; [(op-myandi rd rs1 imm) (myandi rd rs1 imm mem-mystate)]
    ;; [(op-myslli rd rs1 imm) (myslli rd rs1 imm mem-mystate)]
    ;[(op-mysrli rd rs1 imm) (mysrli rd rs1 imm mem-mystate)]
    ;[(op-mysrai rd rs1 imm) (mysrai rd rs1 imm mem-mystate)]
    ;; [(op-myslti rd rs1 imm) (myslti rd rs1 imm mem-mystate)]
    ;; [(op-mysltiu rd rs1 imm) (mysltiu rd rs1 imm mem-mystate)]
    ;my B, J, U
    ))

(define-bounded (execute-program instructions mem-state)
  (let ([pc (cpu-pc mem-state)] [len (length instructions)])
    (cond
    [(< (/ pc 4) len)
     (execute-program instructions (execute-instruction (list-ref instructions (/ pc 4)) mem-state))]
    [else mem-state])))

(define (eq-mem-state memory1 memory2)
  (and
   (equal? (cpu-pc memory1) (cpu-pc memory2))
   (equal? (cpu-registers memory1) (cpu-registers memory2))
   (equal?
    (take (cpu-stack memory1) (- (bitvector->integer (read-register sp memory1))))
    (take (cpu-stack memory2) (- (bitvector->integer (read-register sp memory2))))
   )))
