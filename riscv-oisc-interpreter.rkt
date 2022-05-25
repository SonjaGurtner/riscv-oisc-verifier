#lang rosette
(require threading)
(provide (all-defined-out))

(define XLEN 8)
; int32? is a shorthand for the type (bitvector XLEN).
(define int32? (bitvector XLEN))
(define LOOP-LIM 32)

; int32 takes as input an integer literal and returns the corresponding bitvector
(define (int32 i)
  (bv i int32?))

; Parameter that controls the number of unrollings, for the main execute method (to prevent infinite runs), has to be adjusted
(define fuel (make-parameter 50))

; Syntax rule for a loop that runs until it is out of fuel
(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Limit of loop runs reached")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

; Auxiliary syntax rules which can be used in a ~>> block and work the same way
; They modify the mem-state which is returned by the previous function, and return the new one
(define-syntax ~>>for
  (syntax-rules ()
    ((_ pre bound check body ...)
     (begin
       (let ([x pre])
         (for ([i bound])
           (assert (check i x))
           (set! x (~>> x body ...)))
         x)))))

(define-syntax ~>>for-break
  (syntax-rules ()
    ((_ pre bound check #:break break body ...)
     (begin
       (let ([x pre]
             [stop #f])
         (for ([i bound])
           (if (or (break i x) stop)
               (set! stop #t)
               (begin
                 (assert (check i x))
                 (set! x (~>> x body ...)))))
         x)))))

(define-syntax ~>>when
  (syntax-rules ()
    ((_ pre check body ...)
     (begin
       (let ([x pre])
         (when (check x)
           (set! x (~>> x body ...)))
         x)))))

(define-syntax ~>>if-else
  (syntax-rules ()
    ((_ pre check body1 body2)
     (begin
       (let ([x pre])
         (if (check x)
             (set! x (~>> x body1))
             (set! x (~>> x body2)))
         x)))))

; For debugging purposes, use with (read_and_print_value t1 _)
(define (read_and_print_value rs1 mem-state)
  (display (read-register rs1 mem-state))
  mem-state)

;========================= Define Memory, Register Indices, CPU
(define-values (x0 zero) (values (int32 0) (int32 0)))
(define-values (x1 ra) (values (int32 1) (int32 1)))
(define-values (x2 sp) (values (int32 2) (int32 2)))
(define-values (x3 gp) (values (int32 3) (int32 3)))
(define-values (x4 tp) (values (int32 4) (int32 4)))
(define-values (x5 t0) (values (int32 5) (int32 5)))
(define-values (x6 t1) (values (int32 6) (int32 6)))
(define-values (x7 t2) (values (int32 7) (int32 7)))
(define-values (x8 s0 fp) (values (int32 8) (int32 8) (int32 8)))
(define-values (x9 s1) (values (int32 9) (int32 9)))
(define-values (x10 a0) (values (int32 10) (int32 10)))
(define-values (x11 a1) (values (int32 11) (int32 11)))
(define-values (x12 a2) (values (int32 12) (int32 12)))
(define-values (x13 a3) (values (int32 13) (int32 13)))
(define-values (x14 a4) (values (int32 14) (int32 14)))
(define-values (x15 a5) (values (int32 15) (int32 15)))
(define-values (x16 a6) (values (int32 16) (int32 16)))
(define-values (x17 a7) (values (int32 17) (int32 17)))
(define-values (x18 s2) (values (int32 18) (int32 18)))
(define-values (x19 s3) (values (int32 19) (int32 19)))
(define-values (x20 s4) (values (int32 20) (int32 20)))
(define-values (x21 s5) (values (int32 21) (int32 21)))
(define-values (x22 s6) (values (int32 22) (int32 22)))
(define-values (x23 s7) (values (int32 23) (int32 23)))
(define-values (x24 s8) (values (int32 24) (int32 24)))
(define-values (x25 s9) (values (int32 25) (int32 25)))
(define-values (x26 s10) (values (int32 26) (int32 26)))
(define-values (x27 s11) (values (int32 27) (int32 27)))
(define-values (x28 t3) (values (int32 28) (int32 28)))
(define-values (x29 t4) (values (int32 29) (int32 29)))
(define-values (x30 t5) (values (int32 30) (int32 30)))
(define-values (x31 t6) (values (int32 31) (int32 31)))

(struct cpu (pc registers stack) #:transparent)
(struct instruction ())

;========================= Define op-code for the RISC-V and replaced instructions
; RISC-V instructions
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
;TODO branching and jumps
(struct op-lui (rd imm) #:super struct:instruction)
(struct op-auipc (rd imm) #:super struct:instruction)

; Replaced instructions
(struct op-myadd (rd rs1 rs2) #:super struct:instruction)
(struct op-myxor (rd rs1 rs2) #:super struct:instruction)
(struct op-myor (rd rs1 rs2) #:super struct:instruction)
(struct op-myand (rd rs1 rs2) #:super struct:instruction)
(struct op-mysll (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysrl (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysra (rd rs1 rs2) #:super struct:instruction)
;(struct op-myslt (rd rs1 rs2) #:super struct:instruction)
;(struct op-mysltu (rd rs1 rs2) #:super struct:instruction)
(struct op-myaddi (rd rs1 imm) #:super struct:instruction)
(struct op-myxori (rd rs1 imm) #:super struct:instruction)
(struct op-myori (rd rs1 imm) #:super struct:instruction)
(struct op-myandi (rd rs1 imm) #:super struct:instruction)
(struct op-myslli (rd rs1 imm) #:super struct:instruction)
(struct op-mysrli (rd rs1 imm) #:super struct:instruction)
(struct op-mysrai (rd rs1 imm) #:super struct:instruction)
(struct op-myslti (rd rs1 imm) #:super struct:instruction)
;(struct op-mysltiu (rd rs1 imm) #:super struct:instruction)
;TODO jumps and branching

;========================= Auxiliary Methods
; read and return the value of a register
(define (read-register rd mem-state)
  (list-ref-bv (cpu-registers mem-state) rd))

; write a new value into a register
(define (write-register rd val mem-state)
  (cpu (cpu-pc mem-state) (list-set-bv (cpu-registers mem-state) rd val) (cpu-stack mem-state)))

; increment the register by a given value
(define (increment-register rd val mem-state)
  (write-register rd (bvadd (read-register rd mem-state) val) mem-state))

; increment the pc by 4
(define (increment-pc mem-state)
  (cpu (bvadd (int32 4) (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

; convert the pc to the index which can be used to fetch the respective instruction from the test program
(define (convert-sp-index rd offset mem-state)
  (bvsub (bvlshr (bvsub (bvneg (read-register rd mem-state)) offset) (int32 2)) (int32 1)))

; set the pc to a given value
(define (set-pc pc mem-state)
  (cpu pc (cpu-registers mem-state) (cpu-stack mem-state)))

;========================= RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (sub rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvsub (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

; added rv to the name, as rosette already has xor/or/and defined as functions
(define (rvxor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state) ))

(define (rvor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (rvand rd rs1 rs2 mem-state)
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
    ;(assert (bvslt ind (length-bv (cpu-stack mem-state) (bitvector XLEN))))
    ;(assert (bvslt ind (bvsub (read-register x2 mem-state))))
    (increment-pc (write-register rd (list-ref-bv (cpu-stack mem-state) ind) mem-state))))

;syntax: sw x3, 0(sp) --> sw x3, 0, sp
(define (sw rs1 imm rs2 mem-state)
  (let ([ind (convert-sp-index rs2 imm mem-state)])
    ;(assert (bvslt ind (length-bv (cpu-stack mem-state) (bitvector XLEN))))
    ;(assert (bvslt ind (bvsub (read-register x2 mem-state))))
    (increment-pc
     (cpu
      (cpu-pc mem-state)
      (cpu-registers mem-state)
      (list-set-bv (cpu-stack mem-state) ind (read-register rs1 mem-state))))))

;============== B, J, U - Types
;TODO branching

(define (lui rd imm mem-state)
  (increment-pc (write-register rd (bvshl imm (int32 12)) mem-state)))

(define (auipc rd imm mem-state)
  (increment-pc (write-register rd (bvadd (cpu-pc mem-state) (bvshl imm (int32 12))) mem-state)))

;========================= Replaced Instructions
;============== R-Type
(define (myadd rd rs1 rs2 mem-state)
  (~>> mem-state
       (addi sp sp (int32 -20))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sw rs1 (int32 12) sp)
       (sw rs2 (int32 16) sp)
       (lw t1 (int32 12) sp)
       (lw t0 (int32 16) sp)
       (sub t0 x0 t0)
       (sub t2 t1 t0)
       (sw t2 (int32 16) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 16) sp)
       (addi sp sp (int32 20))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myxor rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
       (if (bveq (read-register rs1 mem-state) (int32 0))
           (~>> mem-state (sub rd x0 rs2) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
           (if (bveq (read-register rs2 mem-state) (int32 0))
               (~>> mem-state (sub rd x0 rs1) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
               (~>> mem-state
                    (addi sp sp (int32 -40))
                    (sw t3 (int32 0) sp)
                    (sw t4 (int32 4) sp)
                    (sw t5 (int32 8) sp)
                    (sw t6 (int32 12) sp)
                    (sw t0 (int32 16) sp)
                    (sw t1 (int32 20) sp)
                    (sw t2 (int32 24) sp)
                    (sw s1 (int32 28) sp)
                    (sw rs1 (int32 32) sp)
                    (sw rs2 (int32 36) sp)
                    (lw t3 (int32 32) sp)
                    (lw t4 (int32 36) sp)
                    (addi t0 x0 (int32 1))
                    (sub t1 x0 x0)
                    (addi t2 x0 (int32 XLEN))
                    (sub s1 x0 x0)
                    (~>>for _ XLEN
                        (λ(i mem) (eq? (int32 i) (read-register t1 mem)))
                        (slli s1 s1 (int32 1))
                        (slli t5 t3 (int32 1))
                        (srli t5 t5 (int32 1))
                        (slli t6 t4 (int32 1))
                        (srli t6 t6 (int32 1))
                        (sub t5 t3 t5)
                        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t5 x0 (int32 1)))
                        (sub t6 t4 t6)
                        (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (int32 0)))) (addi t6 x0 (int32 1)))
                        (add t5 t5 t6)
                        (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t5 mem))) (addi s1 s1 (int32 1)))
                        (addi t1 t1 (int32 1))
                        (slli t3 t3 (int32 1))
                        (slli t4 t4 (int32 1)))
                    (sw s1 (int32 36) sp)
                    (lw s1 (int32 28) sp)
                    (lw t2 (int32 24) sp)
                    (lw t1 (int32 20) sp)
                    (lw t0 (int32 16) sp)
                    (lw t6 (int32 12) sp)
                    (lw t5 (int32 8) sp)
                    (lw t4 (int32 4) sp)
                    (lw t3 (int32 0) sp)
                    (lw rd (int32 36) sp)
                    (addi sp sp (int32 40))
                    (set-pc start-pc)
                    (increment-pc))))))

(define (myor rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
       (if (bveq (read-register rs1 mem-state) (int32 0))
           (~>> mem-state (sub rd x0 rs2) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
           (if (bveq (read-register rs2 mem-state) (int32 0))
               (~>> mem-state (sub rd x0 rs1) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
               (~>> mem-state
                    (addi sp sp (int32 -40))
                    (sw t3 (int32 0) sp)
                    (sw t4 (int32 4) sp)
                    (sw t5 (int32 8) sp)
                    (sw t6 (int32 12) sp)
                    (sw t0 (int32 16) sp)
                    (sw t1 (int32 20) sp)
                    (sw t2 (int32 24) sp)
                    (sw s1 (int32 28) sp)
                    (sw rs1 (int32 32) sp)
                    (sw rs2 (int32 36) sp)
                    (lw t3 (int32 32) sp)
                    (lw t4 (int32 36) sp)
                    (addi t0 x0 (int32 1))
                    (sub t1 x0 x0)
                    (addi t2 x0 (int32 XLEN))
                    (sub s1 x0 x0)
                    (~>>for _ XLEN
                        (λ(i mem) (eq? (int32 i) (read-register t1 mem)))
                        (slli s1 s1 (int32 1))
                        (slli t5 t3 (int32 1))
                        (srli t5 t5 (int32 1))
                        (slli t6 t4 (int32 1))
                        (srli t6 t6 (int32 1))
                        (sub t5 t3 t5)
                        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t5 x0 (int32 1)))
                        (sub t6 t4 t6)
                        (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (int32 0)))) (addi t6 x0 (int32 1)))
                        (add t5 t5 t6)
                        (~>>when _ (λ(mem) (bvslt (int32 0) (read-register t5 mem))) (addi s1 s1 (int32 1)))
                        (addi t1 t1 (int32 1))
                        (slli t3 t3 (int32 1))
                        (slli t4 t4 (int32 1)))
                    (sw s1 (int32 36) sp)
                    (lw s1 (int32 28) sp)
                    (lw t2 (int32 24) sp)
                    (lw t1 (int32 20) sp)
                    (lw t0 (int32 16) sp)
                    (lw t6 (int32 12) sp)
                    (lw t5 (int32 8) sp)
                    (lw t4 (int32 4) sp)
                    (lw t3 (int32 0) sp)
                    (lw rd (int32 36) sp)
                    (addi sp sp (int32 40))
                    (set-pc start-pc)
                    (increment-pc))))))

(define (myand rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
       (if (bveq (read-register rs1 mem-state) (int32 0))
           (~>> mem-state (sub rd x0 x0) (set-pc start-pc) (increment-pc))
           (if (bveq (read-register rs2 mem-state) (int32 0))
               (~>> mem-state (sub rd x0 x0) (set-pc start-pc) (increment-pc))
               (~>> mem-state
                    (addi sp sp (int32 -40))
                    (sw t3 (int32 0) sp)
                    (sw t4 (int32 4) sp)
                    (sw t5 (int32 8) sp)
                    (sw t6 (int32 12) sp)
                    (sw t0 (int32 16) sp)
                    (sw t1 (int32 20) sp)
                    (sw t2 (int32 24) sp)
                    (sw s1 (int32 28) sp)
                    (sw rs1 (int32 32) sp)
                    (sw rs2 (int32 36) sp)
                    (lw t3 (int32 32) sp)
                    (lw t4 (int32 36) sp)
                    (addi t0 x0 (int32 2))
                    (sub t1 x0 x0)
                    (addi t2 x0 (int32 XLEN))
                    (sub s1 x0 x0)
                    (~>>for _ XLEN
                        (λ(i mem) (eq? (int32 i) (read-register t1 mem)))
                        (slli s1 s1 (int32 1))
                        (slli t5 t3 (int32 1))
                        (srli t5 t5 (int32 1))
                        (slli t6 t4 (int32 1))
                        (srli t6 t6 (int32 1))
                        (sub t5 t3 t5)
                        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t5 x0 (int32 1)))
                        (sub t6 t4 t6)
                        (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (int32 0)))) (addi t6 x0 (int32 1)))
                        (add t5 t5 t6)
                        (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t5 mem))) (addi s1 s1 (int32 1)))
                        (addi t1 t1 (int32 1))
                        (slli t3 t3 (int32 1))
                        (slli t4 t4 (int32 1)))
                    (sw s1 (int32 36) sp)
                    (lw s1 (int32 28) sp)
                    (lw t2 (int32 24) sp)
                    (lw t1 (int32 20) sp)
                    (lw t0 (int32 16) sp)
                    (lw t6 (int32 12) sp)
                    (lw t5 (int32 8) sp)
                    (lw t4 (int32 4) sp)
                    (lw t3 (int32 0) sp)
                    (lw rd (int32 36) sp)
                    (addi sp sp (int32 40))
                    (set-pc start-pc)
                    (increment-pc))))))

(define (mysll rd rs1 rs2 mem-state)
  (~>> mem-state
       (addi sp sp (int32 -16))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw rs1 (int32 8) sp)
       (sw rs2 (int32 12) sp)
       (lw t1 (int32 8) sp)
       (lw t0 (int32 12) sp)
       (andi t0 t0 (int32 31))
       (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (int32 0))) (add rd x0 t1) (sll rd t1 t0))
       (sw rd (int32 12) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 12) sp)
       (addi sp sp (int32 16))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (mysll-safe rd rs1 rs2 mem-state)
  (~>> mem-state
       (addi sp sp (int32 -24))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sw t3 (int32 12) sp)
       (sw rs1 (int32 16) sp)
       (sw rs2 (int32 20) sp)
       (lw t3 (int32 16) sp)
       (lw t1 (int32 20) sp)
       (sub t2 x0 x0)
       (sub t0 x0 t3)
       (~>>for-break _ LOOP-LIM
          (λ(i mem) (eq? (int32 i) (read-register t2 mem)))
          #:break (λ(i mem) (bveq (int32 i) (read-register t1 mem)))
          (sub t3 t3 t0)
          (sub t0 x0 t3)
          (addi t2 t2 (int32 1)))
       (sw t3 (int32 20) sp)
       (lw t3 (int32 12) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 20) sp)
       (addi sp sp (int32 24))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

;============== I-Type
(define (myaddi rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -16))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sub t1 x0 rs1)
       (sub t1 x0 t1)
       (addi t0 x0 imm)
       (sub t0 x0 t0)
       (sub t2 t1 t0)
       (sw t2 (int32 12) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 12) sp)
       (addi sp sp (int32 16))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myxori rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sub t1 x0 rs1)
       (sub t1  x0  t1)
       (addi t0  x0  imm)
       (rvxor rd  t1  t0)
       (sw rd  (int32 8) sp)
       (lw t1  (int32 4) sp)
       (lw t0  (int32 0) sp)
       (lw rd  (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myori rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sub t1 x0 rs1)
       (sub t1  x0  t1)
       (addi t0  x0  imm)
       (rvor rd  t1  t0)
       (sw rd  (int32 8) sp)
       (lw t1  (int32 4) sp)
       (lw t0  (int32 0) sp)
       (lw rd  (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myandi rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sub t1 x0 rs1)
       (sub t1  x0  t1)
       (addi t0  x0  imm)
       (rvand rd  t1  t0)
       (sw rd  (int32 8) sp)
       (lw t1  (int32 4) sp)
       (lw t0  (int32 0) sp)
       (lw rd  (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myslli rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw rs1 (int32 8) sp)
       (lw t1 (int32 8) sp)
       (addi t0 x0 imm)
       (andi t0 t0 (int32 31))
       (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (int32 0))) (add rd x0 t1) (sll rd t1 t0))
       (sw rd (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myslli-safe rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -20))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sw t3 (int32 12) sp)
       (sw rs1 (int32 16) sp)
       (lw t3 (int32 16) sp)
       (sub t0 x0 t3)
       (addi t1 x0 imm)
       (sub t2 x0 x0)
       (~>>for-break _ LOOP-LIM
          (λ(i mem) (eq? (int32 i) (read-register t2 mem)))
          #:break (λ(i mem) (bveq (int32 i) (read-register t1 mem)))
          (sub t3 t3 t0)
          (sub t0 x0 t3)
          (addi t2 t2 (int32 1)))
       (sw t3 (int32 16) sp)
       (lw t3 (int32 12) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 16) sp)
       (addi sp sp (int32 20))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (mysrli rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw rs1 (int32 8) sp)
       (lw t1 (int32 8) sp)
       (addi t0 x0 imm)
       (andi t0 t0 (int32 31))
       (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (int32 0))) (add rd x0 t1) (srl rd t1 t0))
       (sw rd (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (mysrli-safe rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -28))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sw t3 (int32 12) sp)
       (sw t4 (int32 16) sp)
       (sw t5 (int32 20) sp)
       (sw rs1 (int32 24) sp)
       (addi t2 x0 imm)
       (addi t1 x0 (int32 XLEN))
       (sub t1 t1 t2)
       (sub t2 x0 x0)
       (lw t0 (int32 24) sp)
       (sub t3 x0 x0)
       (lw t4 (int32 24) sp)
       (slli t4 t4 (int32 1))
       (srli t4 t4 (int32 1))
       (~>>for-break _ LOOP-LIM
          (λ(i mem) (eq? (int32 i) (read-register t2 mem)))
          #:break (λ(i mem) (bveq (int32 i) (read-register t1 mem)))
          (slli t4 t4 (int32 1))
          (srli t4 t4 (int32 1))
          (slli t3 t3 (int32 1))
          (sub t5 t0 t4)
          (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t3 t3 (int32 1)))
          (addi t2 t2 (int32 1))
          (slli t0 t0 (int32 1))
          (slli t4 t4 (int32 1)))
       (sw t3 (int32 24) sp)
       (lw t5 (int32 20) sp)
       (lw t4 (int32 16) sp)
       (lw t3 (int32 12) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 24) sp)
       (addi sp sp (int32 28))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (mysrai rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw rs1 (int32 8) sp)
       (lw t1 (int32 8) sp)
       (addi t0 x0 imm)
       (andi t0 t0 (int32 31))
       (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (int32 0))) (add rd x0 t1) (sra rd t1 t0))
       (sw rd (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (mysrai-safe rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -28))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw t2 (int32 8) sp)
       (sw t3 (int32 12) sp)
       (sw t4 (int32 16) sp)
       (sw t5 (int32 20) sp)
       (sw rs1 (int32 24) sp)
       (addi t1 x0 imm)
       (lw t0 (int32 24) sp)
       (lw t4 (int32 24) sp)
       (sub t2 x0 x0)
       (sub t3 x0 x0)
       (slli t4 t4 (int32 1))
       (srli t4 t4 (int32 1))
       (sub t5 t0 t4)
       (lw t4 (int32 24) sp)
       (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t5 x0 (int32 1)))
       (~>>for-break _ LOOP-LIM
          (λ(i mem) (eq? (int32 i) (read-register t2 mem)))
          #:break (λ(i mem) (bveq (int32 i) (read-register t1 mem)))
          (slli t3 t3 (int32 1))
          (add t3 t3 t5)
          (addi t2 t2 (int32 1)))
       (addi t2 x0 (int32 XLEN))
       (sub t1 t2 t1)
       (sub t2 t2 t2)
       (~>>for-break _ LOOP-LIM
          (λ(i mem) (eq? (int32 i) (read-register t2 mem)))
          #:break (λ(i mem) (bveq (int32 i) (read-register t1 mem)))
          (slli t4 t4 (int32 1))
          (srli t4 t4 (int32 1))
          (slli t3 t3 (int32 1))
          (sub t5 t0 t4)
          (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (int32 0)))) (addi t3 t3 (int32 1)))
          (addi t2 t2 (int32 1))
          (slli t0 t0 (int32 1))
          (slli t4 t4 (int32 1)))
       (sw t3 (int32 24) sp)
       (lw t5 (int32 20) sp)
       (lw t4 (int32 16) sp)
       (lw t3 (int32 12) sp)
       (lw t2 (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 24) sp)
       (addi sp sp (int32 28))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myslti rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (int32 -12))
       (sw t0 (int32 0) sp)
       (sw t1 (int32 4) sp)
       (sw rs1 (int32 8) sp)
       (lw t1 (int32 8) sp)
       (addi t0 x0 imm)
       (slt rd t1 t0)
       (sw rd (int32 8) sp)
       (lw t1 (int32 4) sp)
       (lw t0 (int32 0) sp)
       (lw rd (int32 8) sp)
       (addi sp sp (int32 12))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))


;============== Memory
;============== B, J, U - Types

;========================= Main Functions

(define (execute-instruction instruction mem-state)
  (match instruction
    ; RISC-V Instructions
    [(op-add rd rs1 rs2) (add rd rs1 rs2 mem-state)]
    [(op-sub rd rs1 rs2) (sub rd rs1 rs2 mem-state)]
    [(op-xor rd rs1 rs2) (rvxor rd rs1 rs2 mem-state)]
    [(op-or rd rs1 rs2) (rvor rd rs1 rs2 mem-state)]
    [(op-and rd rs1 rs2) (rvand rd rs1 rs2 mem-state)]
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
    ;TODO B, J , U
    [(op-lui rd imm) (lui rd imm mem-state)]
    [(op-auipc rd imm) (auipc rd imm mem-state)]
    ; Replaced Instructions
    [(op-myadd rd rs1 rs2) (myadd rd rs1 rs2 mem-state)]
    [(op-myxor rd rs1 rs2) (myxor rd rs1 rs2 mem-state)]
    [(op-myor rd rs1 rs2) (myor rd rs1 rs2 mem-state)]
    [(op-myand rd rs1 rs2) (myand rd rs1 rs2 mem-state)]
    [(op-mysll rd rs1 rs2) (mysll rd rs1 rs2 mem-state)]
    ;[(op-mysrl rd rs1 rs2) (mysrl rd rs1 rs2 mem-state)]
    ;[(op-mysra rd rs1 rs2) (mysra rd rs1 rs2 mem-state)]
    ;; [(op-myslt rd rs1 rs2) (myslt rd rs1 rs2 mem-state)]
    ;; [(op-mysltu rd rs1 rs2) (mysltu rd rs1 rs2 mem-state)]
    [(op-myaddi rd rs1 imm) (myaddi rd rs1 imm mem-state)]
    [(op-myxori rd rs1 imm) (myxori rd rs1 imm mem-state)]
    [(op-myori rd rs1 imm) (myori rd rs1 imm mem-state)]
    [(op-myandi rd rs1 imm) (myandi rd rs1 imm mem-state)]
    [(op-myslli rd rs1 imm) (myslli rd rs1 imm mem-state)]
    [(op-mysrli rd rs1 imm) (mysrli rd rs1 imm mem-state)]
    [(op-mysrai rd rs1 imm) (mysrai rd rs1 imm mem-state)]
    [(op-myslti rd rs1 imm) (myslti rd rs1 imm mem-state)]
    ;; [(op-mysltiu rd rs1 imm) (mysltiu rd rs1 imm mem-state)]
    ;TODO B, J, U
    ))

(define-bounded (execute-program instructions mem-state)
  (let ([pc (cpu-pc mem-state)] [len (length-bv instructions (bitvector XLEN))])
    (cond
      [(bvslt (bvlshr pc (int32 2)) len)
       (display (bvlshr pc (int32 2)))
       (execute-program instructions (execute-instruction (list-ref-bv instructions (bvlshr pc (int32 2))) mem-state))]
      [else mem-state])))

(define (eq-mem-state memory1 memory2)
  (and
   (equal? (cpu-pc memory1) (cpu-pc memory2))
   (equal? (cpu-registers memory1) (cpu-registers memory2))
   (equal?
    (take-bv (cpu-stack memory1) (bvlshr (bvneg (read-register sp memory1)) (int32 2)))
    (take-bv (cpu-stack memory2) (bvlshr (bvneg (read-register sp memory2)) (int32 2))))))
