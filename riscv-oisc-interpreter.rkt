#lang rosette
(require threading)
(provide (all-defined-out))

; Number of bits
(define XLEN 8)
; intXLEN? is a shorthand for the type (bitvector XLEN).
(define intXLEN? (bitvector XLEN))
; Some loops need a limit higher than XLEN(i.e. right shift)
(define LOOP-LIM 32)

; intXLEN takes as input an integer and returns the respective bitvector of length XLEN
(define (intXLEN i)
  (bv i intXLEN?))

; Parameter that controls the number of unrollings, for the main execute method (to prevent infinite runs), has to be adjusted
(define fuel (make-parameter 50))

; Syntax rule for a loop that runs until it is out of fuel
(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Limit of loop runs reached")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

; Auxiliary syntax rules which can be used in a ~>> block
; They take as input the mem-state which is returned by the previous function (pre), modify it, and return the new one
; For loop that executes the body until the bound is reached, and asserts that the check assertions hold
(define-syntax ~>>for
  (syntax-rules ()
    ((_ pre bound check body ...)
     (begin
       (let ([x pre])
         (for ([i bound])
           (assert (check i x))
           (set! x (~>> x body ...)))
         x)))))

; Also a for loop, which breaks before the bound is reached if the break condition is true
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

; Execute the body if the check condition holds
(define-syntax ~>>when
  (syntax-rules ()
    ((_ pre check body ...)
     (begin
       (let ([x pre])
         (when (check x)
           (set! x (~>> x body ...)))
         x)))))

; Execute body1 if the check holds, body2 otherwise (can only execute one instruction each)
(define-syntax ~>>if-else
  (syntax-rules ()
    ((_ pre check body1 body2)
     (begin
       (let ([x pre])
         (if (check x)
             (set! x (~>> x body1))
             (set! x (~>> x body2)))
         x)))))

; For better debugging
(define (read-and-print-value str rs1 mem-state)
  (display (format "~a ~a" str (read-register rs1 mem-state)))
  mem-state)

; Print all register and stack values for better readability, stack will not be printed if sp == 0
; k range in the stack loop needs to be adapted if stack size is changed
(define (print-all-memory mem-state)
  (displayln (format "PC: ~a" (cpu-pc mem-state)))
  (displayln "Memory (Registers)")
  (for ([k (range 32)] [i (cpu-registers mem-state)])
    (displayln (format "x~a ~a" k i )))
  (displayln "Memory (Stack)")
  (for ([k (range 32)] [i (cpu-stack mem-state)])
    (when (< k (bitvector->integer (read-register sp mem-state)))
      (displayln (format "~a" i)))))

(define (print-all-memory-many mem1 mem2)
  (displayln (format "PC: ~a \t2 ~a" (cpu-pc mem1) (cpu-pc mem2)))
  (displayln "Memory (Registers)")
  (for ([k (range 32)] [i (cpu-registers mem1)] [j (cpu-registers mem2)])
    (displayln (format "x~a ~a \t2 ~a \t ~a" k i j (if (bveq i j) "" "DIFFERENT"))))
  (displayln "Memory (Stack)")
  (for ([k (range 32)] [i (cpu-stack mem1)] [j (cpu-stack mem2)])
    (when (< k (bitvector->integer (read-register sp mem1)))
      (displayln (format "~a \t2 ~a \t ~a" i j (if (bveq i j) "" "DIFFERENT"))))))

;========================= Define Memory, Register Indices, CPU
(define-values (x0 zero) (values (intXLEN 0) (intXLEN 0)))
(define-values (x1 ra) (values (intXLEN 1) (intXLEN 1)))
(define-values (x2 sp) (values (intXLEN 2) (intXLEN 2)))
(define-values (x3 gp) (values (intXLEN 3) (intXLEN 3)))
(define-values (x4 tp) (values (intXLEN 4) (intXLEN 4)))
(define-values (x5 t0) (values (intXLEN 5) (intXLEN 5)))
(define-values (x6 t1) (values (intXLEN 6) (intXLEN 6)))
(define-values (x7 t2) (values (intXLEN 7) (intXLEN 7)))
(define-values (x8 s0 fp) (values (intXLEN 8) (intXLEN 8) (intXLEN 8)))
(define-values (x9 s1) (values (intXLEN 9) (intXLEN 9)))
(define-values (x10 a0) (values (intXLEN 10) (intXLEN 10)))
(define-values (x11 a1) (values (intXLEN 11) (intXLEN 11)))
(define-values (x12 a2) (values (intXLEN 12) (intXLEN 12)))
(define-values (x13 a3) (values (intXLEN 13) (intXLEN 13)))
(define-values (x14 a4) (values (intXLEN 14) (intXLEN 14)))
(define-values (x15 a5) (values (intXLEN 15) (intXLEN 15)))
(define-values (x16 a6) (values (intXLEN 16) (intXLEN 16)))
(define-values (x17 a7) (values (intXLEN 17) (intXLEN 17)))
(define-values (x18 s2) (values (intXLEN 18) (intXLEN 18)))
(define-values (x19 s3) (values (intXLEN 19) (intXLEN 19)))
(define-values (x20 s4) (values (intXLEN 20) (intXLEN 20)))
(define-values (x21 s5) (values (intXLEN 21) (intXLEN 21)))
(define-values (x22 s6) (values (intXLEN 22) (intXLEN 22)))
(define-values (x23 s7) (values (intXLEN 23) (intXLEN 23)))
(define-values (x24 s8) (values (intXLEN 24) (intXLEN 24)))
(define-values (x25 s9) (values (intXLEN 25) (intXLEN 25)))
(define-values (x26 s10) (values (intXLEN 26) (intXLEN 26)))
(define-values (x27 s11) (values (intXLEN 27) (intXLEN 27)))
(define-values (x28 t3) (values (intXLEN 28) (intXLEN 28)))
(define-values (x29 t4) (values (intXLEN 29) (intXLEN 29)))
(define-values (x30 t5) (values (intXLEN 30) (intXLEN 30)))
(define-values (x31 t6) (values (intXLEN 31) (intXLEN 31)))

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
(struct op-beq (rs1 rs2 imm) #:super struct:instruction)
(struct op-bne (rs1 rs2 imm) #:super struct:instruction)
(struct op-blt (rs1 rs2 imm) #:super struct:instruction)
(struct op-bge (rs1 rs2 imm) #:super struct:instruction)
(struct op-bltu (rs1 rs2 imm) #:super struct:instruction)
(struct op-bgeu (rs1 rs2 imm) #:super struct:instruction)
(struct op-jal (rd imm) #:super struct:instruction)
(struct op-jalr (rd rs1 imm) #:super struct:instruction)
(struct op-lui (rd imm) #:super struct:instruction)
(struct op-auipc (rd imm) #:super struct:instruction)

; Replaced instructions
(struct op-myadd (rd rs1 rs2) #:super struct:instruction)
(struct op-myxor (rd rs1 rs2) #:super struct:instruction)
(struct op-myor (rd rs1 rs2) #:super struct:instruction)
(struct op-myand (rd rs1 rs2) #:super struct:instruction)
(struct op-mysll (rd rs1 rs2) #:super struct:instruction)
(struct op-mysrl (rd rs1 rs2) #:super struct:instruction)
(struct op-mysra (rd rs1 rs2) #:super struct:instruction)
(struct op-myslt (rd rs1 rs2) #:super struct:instruction)
(struct op-mysltu (rd rs1 rs2) #:super struct:instruction)
(struct op-myaddi (rd rs1 imm) #:super struct:instruction)
(struct op-myxori (rd rs1 imm) #:super struct:instruction)
(struct op-myori (rd rs1 imm) #:super struct:instruction)
(struct op-myandi (rd rs1 imm) #:super struct:instruction)
(struct op-myslli (rd rs1 imm) #:super struct:instruction)
(struct op-mysrli (rd rs1 imm) #:super struct:instruction)
(struct op-mysrai (rd rs1 imm) #:super struct:instruction)
(struct op-myslti (rd rs1 imm) #:super struct:instruction)
(struct op-mysltiu (rd rs1 imm) #:super struct:instruction)
(struct op-mybne (rs1 rs2 imm) #:super struct:instruction)
(struct op-mybge (rs1 rs2 imm) #:super struct:instruction)
(struct op-mybltu (rs1 rs2 imm) #:super struct:instruction)
(struct op-mybgeu (rs1 rs2 imm) #:super struct:instruction)
(struct op-myjal (rd imm) #:super struct:instruction)

;========================= Auxiliary Methods
; Read and return the value of a register
(define (read-register rd mem-state)
  (list-ref-bv (cpu-registers mem-state) rd))

; Write a new value into a register
(define (write-register rd val mem-state)
  (cpu (cpu-pc mem-state) (list-set-bv (cpu-registers mem-state) rd val) (cpu-stack mem-state)))

; Increment the register by a given value
(define (increment-register rd val mem-state)
  (write-register rd (bvadd (read-register rd mem-state) val) mem-state))

; Increment the pc by 4, goes to next instruction
(define (increment-pc mem-state)
  (cpu (bvadd (intXLEN 4) (cpu-pc mem-state)) (cpu-registers mem-state)  (cpu-stack mem-state)))

; Convert the sp and offset to the index of the respective position in the stack
(define (convert-sp-index rd offset mem-state)
  (bvsub (bvlshr (bvsub (bvneg (read-register rd mem-state)) offset) (intXLEN 2)) (intXLEN 1)))

; Set the pc to a given value
(define (set-pc pc mem-state)
  (cpu pc (cpu-registers mem-state) (cpu-stack mem-state)))

;========================= RISC-V Instructions
;============== R-Type
(define (add rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvadd (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (sub rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvsub (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

; Added rv to the name, as rosette already has xor/or/and defined as functions
(define (rvxor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvxor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state) ))

(define (rvor rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvor (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

(define (rvand rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvand (read-register rs1 mem-state) (read-register rs2 mem-state)) mem-state)))

; The shift amount in RISC-V is 5 bits long, therefore the 5 lowest bits get extracted with (bvand x 31)
(define (sll rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (intXLEN 31))) mem-state)))

(define (srl rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (intXLEN 31))) mem-state)))

(define (sra rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand (read-register rs2 mem-state) (intXLEN 31))) mem-state)))

(define (slt rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) (read-register rs2 mem-state)) (intXLEN 1) (intXLEN 0)) mem-state)))

(define (sltu rd rs1 rs2 mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) (read-register rs2 mem-state)) (intXLEN 1) (intXLEN 0)) mem-state)))

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
  (increment-pc (write-register rd (bvshl (read-register rs1 mem-state) (bvand imm (intXLEN 31))) mem-state)))

(define (srli rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvlshr (read-register rs1 mem-state) (bvand imm (intXLEN 31))) mem-state)))

(define (srai rd rs1 imm mem-state)
  (increment-pc (write-register rd (bvashr (read-register rs1 mem-state) (bvand imm (intXLEN 31))) mem-state)))

(define (slti rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvslt (read-register rs1 mem-state) imm) (intXLEN 1) (intXLEN 0)) mem-state)))

(define (sltiu rd rs1 imm mem-state)
  (increment-pc (write-register rd (if (bvult (read-register rs1 mem-state) imm) (intXLEN 1) (intXLEN 0)) mem-state)))

;============== Jumps and Branching
(define (beq rs1 rs2 imm mem-state)
  (if (bveq (read-register rs1 mem-state) (read-register rs2 mem-state))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (bne rs1 rs2 imm mem-state)
  (if (not (bveq (read-register rs1 mem-state) (read-register rs2 mem-state)))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (blt rs1 rs2 imm mem-state)
  (if (bvslt (read-register rs1 mem-state) (read-register rs2 mem-state))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (bge rs1 rs2 imm mem-state)
  (if (bvsge (read-register rs1 mem-state) (read-register rs2 mem-state))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (bltu rs1 rs2 imm mem-state)
  (if (bvult (read-register rs1 mem-state) (read-register rs2 mem-state))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (bgeu rs1 rs2 imm mem-state)
  (if (bvuge (read-register rs1 mem-state) (read-register rs2 mem-state))
      (set-pc (bvadd (cpu-pc mem-state) imm) mem-state)
      (increment-pc mem-state)))

(define (jal rd imm mem-state)
  (~>> mem-state
     (addi rd x0 (cpu-pc mem-state))
     (addi rd rd (intXLEN 4))
     (set-pc (bvadd (cpu-pc mem-state) imm))))

(define (jalr rd rs1 imm mem-state)
  (~>> mem-state
     (addi rd x0 (cpu-pc mem-state))
     (addi rd rd (intXLEN 4))
     (set-pc (bvadd (read-register rs1) imm))))

(define (lui rd imm mem-state)
  (increment-pc (write-register rd (bvshl imm (intXLEN 12)) mem-state)))

(define (auipc rd imm mem-state)
  (increment-pc (write-register rd (bvadd (cpu-pc mem-state) (bvshl imm (intXLEN 12))) mem-state)))

;============== Memory
; syntax: lw x3, 0(sp) --> lw x3, 0, sp
(define (lw rd imm rs1 mem-state)
  (let ([ind (convert-sp-index rs1 imm mem-state)])
    (increment-pc (write-register rd (list-ref-bv (cpu-stack mem-state) ind) mem-state))))

(define (sw rs1 imm rs2 mem-state)
  (let ([ind (convert-sp-index rs2 imm mem-state)])
    (increment-pc
     (cpu
      (cpu-pc mem-state)
      (cpu-registers mem-state)
      (list-set-bv (cpu-stack mem-state) ind (read-register rs1 mem-state))))))

;========================= Replaced Instructions
;============== R-Type
(define (myadd rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -20))				;; preserve space on stack
     (sw t0 (intXLEN 0) sp)				;; save registers
     (sw t1 (intXLEN 4) sp)
     (sw t2 (intXLEN 8) sp)
     (sw rs1 (intXLEN 12) sp)
     (sw rs2 (intXLEN 16) sp)
     (lw t1 (intXLEN 12) sp)				;; t1 = rs1
     (lw t0 (intXLEN 16) sp)				;; t0 = rs2
     (sub t0 x0 t0)					;; t0 = -rs2
     (sub t2 t1 t0)					;; t2 = rs1-(-rs2)
     (sw t2 (intXLEN 16) sp)
     (lw t2 (intXLEN 8) sp)				;; restore registers
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 16) sp)
     (addi sp sp (intXLEN 20))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (myxor rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
    ;; special cases, if one of the two source registers is 0, then the result is the value of the other register
    (if (bveq (read-register rs1 mem-state) (intXLEN 0))
        (~>> mem-state (sub rd x0 rs2) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
        (if (bveq (read-register rs2 mem-state) (intXLEN 0))
            (~>> mem-state (sub rd x0 rs1) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
            (~>> mem-state
               (addi sp sp (intXLEN -40))
               (sw t3 (intXLEN 0) sp)			;; shifting rs1
               (sw t4 (intXLEN 4) sp)			;; shifting rs2
               (sw t5 (intXLEN 8) sp)			;; MSB eliminated rs1
               (sw t6 (intXLEN 12) sp)			;; MSB eliminated rs2
               (sw t0 (intXLEN 16) sp)			;; 1 for comparing MSBs
               (sw t1 (intXLEN 20) sp)			;; loop counter i
               (sw t2 (intXLEN 24) sp)			;; loop limit
               (sw s1 (intXLEN 28) sp)
               (sw rs1 (intXLEN 32) sp)
               (sw rs2 (intXLEN 36) sp)
               (lw t3 (intXLEN 32) sp)			;; t3 = rs1
               (lw t4 (intXLEN 36) sp)			;; t4 = rs2
               (addi t0 x0 (intXLEN 1))			;; for comparing sum of MSBs
               (sub t1 x0 x0)				;; i starts with 0
               (addi t2 x0 (intXLEN XLEN))		;; loop limit = XLEN
               (sub s1 x0 x0)
               ;; Rosette cannot handle unbounded loops, therefore XLEN is used as bound (as we compare every bit)
               ;; First check is that i is always the same value as t1 (which is used in the loop as i)
               (~>>for _ XLEN
                  (λ(i mem) (eq? (intXLEN i) (read-register t1 mem)))
                  (slli s1 s1 (intXLEN 1))		;; shift the result left by 1
                  (slli t5 t3 (intXLEN 1))
                  (srli t5 t5 (intXLEN 1))		;; eliminated MSB of rs1
                  (slli t6 t4 (intXLEN 1))
                  (srli t6 t6 (intXLEN 1))		;; eliminated MSB of rs2
                  (sub t5 t3 t5)
                  ;; if the result of subtracting the shifted version from the original one, the MSB is 1
                  (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t5 x0 (intXLEN 1)))
                  (sub t6 t4 t6)
                  (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (intXLEN 0)))) (addi t6 x0 (intXLEN 1)))
                  (add t5 t5 t6)
                  ;; sum of MSBs has to be exactly one (0^1, 1^0)
                  (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t5 mem))) (addi s1 s1 (intXLEN 1)))
                  (addi t1 t1 (intXLEN 1))
                  (slli t3 t3 (intXLEN 1))		;; shift rs1 and rs2 left by 1 to compare the next bit
                  (slli t4 t4 (intXLEN 1)))
               (sw s1 (intXLEN 36) sp)
               (lw s1 (intXLEN 28) sp)
               (lw t2 (intXLEN 24) sp)
               (lw t1 (intXLEN 20) sp)
               (lw t0 (intXLEN 16) sp)
               (lw t6 (intXLEN 12) sp)
               (lw t5 (intXLEN 8) sp)
               (lw t4 (intXLEN 4) sp)
               (lw t3 (intXLEN 0) sp)
               (lw rd (intXLEN 36) sp)
               (addi sp sp (intXLEN 40))
               (set-pc start-pc)
               (increment-pc))))))

(define (myor rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
    ;; special cases, if one of the registers is 0 then just return the other one
    (if (bveq (read-register rs1 mem-state) (intXLEN 0))
        (~>> mem-state (sub rd x0 rs2) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
        (if (bveq (read-register rs2 mem-state) (intXLEN 0))
            (~>> mem-state (sub rd x0 rs1) (sub rd x0 rd) (set-pc start-pc) (increment-pc))
            (~>> mem-state
               (addi sp sp (intXLEN -40))
               (sw t3 (intXLEN 0) sp)			;; shifting rs1
               (sw t4 (intXLEN 4) sp)			;; shifting rs2
               (sw t5 (intXLEN 8) sp)			;; MSB eliminated rs1
               (sw t6 (intXLEN 12) sp)			;; MSB eliminated rs2
               (sw t0 (intXLEN 16) sp)			;; 1 for comparing MSBs
               (sw t1 (intXLEN 20) sp)			;; loop counter i
               (sw t2 (intXLEN 24) sp)			;; loop limit
               (sw s1 (intXLEN 28) sp)
               (sw rs1 (intXLEN 32) sp)
               (sw rs2 (intXLEN 36) sp)
               (lw t3 (intXLEN 32) sp)			;; t3 = rs1
               (lw t4 (intXLEN 36) sp)			;; t4 = rs2
               (addi t0 x0 (intXLEN 1))			;; for comparing sum of MSBs
               (sub t1 x0 x0)
               (addi t2 x0 (intXLEN XLEN))		;; loop limit is XLEN
               (sub s1 x0 x0)
               (~>>for _ XLEN
                  (λ(i mem) (eq? (intXLEN i) (read-register t1 mem)))
                  (slli s1 s1 (intXLEN 1))
                  (slli t5 t3 (intXLEN 1))		;; eliminate MSB
                  (srli t5 t5 (intXLEN 1))
                  (slli t6 t4 (intXLEN 1))
                  (srli t6 t6 (intXLEN 1))
                  (sub t5 t3 t5)
                  ;; if t5 == 0 then MSB = 0, else MSB = 1
                  (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t5 x0 (intXLEN 1)))
                  (sub t6 t4 t6)
                  (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (intXLEN 0)))) (addi t6 x0 (intXLEN 1)))
                  (add t5 t5 t6)
                  ;; sum of MSBs has to be at least 1 (0|1, 1|0, 1|1)
                  (~>>when _ (λ(mem) (bvslt (intXLEN 0) (read-register t5 mem))) (addi s1 s1 (intXLEN 1)))
                  (addi t1 t1 (intXLEN 1))		;; i++
                  (slli t3 t3 (intXLEN 1))
                  (slli t4 t4 (intXLEN 1)))
               (sw s1 (intXLEN 36) sp)
               (lw s1 (intXLEN 28) sp)
               (lw t2 (intXLEN 24) sp)
               (lw t1 (intXLEN 20) sp)
               (lw t0 (intXLEN 16) sp)
               (lw t6 (intXLEN 12) sp)
               (lw t5 (intXLEN 8) sp)
               (lw t4 (intXLEN 4) sp)
               (lw t3 (intXLEN 0) sp)
               (lw rd (intXLEN 36) sp)
               (addi sp sp (intXLEN 40))
               (set-pc start-pc)
               (increment-pc))))))

(define (myand rd rs1 rs2 mem-state)
  (let ([start-pc (cpu-pc mem-state)])
    (if (bveq (read-register rs1 mem-state) (intXLEN 0))
        (~>> mem-state (sub rd x0 x0) (set-pc start-pc) (increment-pc))
        (if (bveq (read-register rs2 mem-state) (intXLEN 0))
            (~>> mem-state (sub rd x0 x0) (set-pc start-pc) (increment-pc))
            (~>> mem-state
               (addi sp sp (intXLEN -40))
               (sw t3 (intXLEN 0) sp)			;; shifting rs1
               (sw t4 (intXLEN 4) sp)			;; shifting rs2
               (sw t5 (intXLEN 8) sp)			;; MSB eliminated rs1
               (sw t6 (intXLEN 12) sp)			;; MSB eliminated rs2
               (sw t0 (intXLEN 16) sp)			;; 1 for comparing MSBs
               (sw t1 (intXLEN 20) sp)			;; loop counter i
               (sw t2 (intXLEN 24) sp)			;; loop limit
               (sw s1 (intXLEN 28) sp)
               (sw rs1 (intXLEN 32) sp)
               (sw rs2 (intXLEN 36) sp)
               (lw t3 (intXLEN 32) sp)			;; t3 = rs1
               (lw t4 (intXLEN 36) sp)			;; t4 = rs2
               (addi t0 x0 (intXLEN 2))			;; for comparing sum of MSBs
               (sub t1 x0 x0)
               (addi t2 x0 (intXLEN XLEN))
               (sub s1 x0 x0)
               (~>>for _ XLEN
                  (λ(i mem) (eq? (intXLEN i) (read-register t1 mem)))
                  (slli s1 s1 (intXLEN 1))
                  (slli t5 t3 (intXLEN 1))		;; eliminate MSB
                  (srli t5 t5 (intXLEN 1))
                  (slli t6 t4 (intXLEN 1))
                  (srli t6 t6 (intXLEN 1))
                  (sub t5 t3 t5)
                  ;; if t5 == 0 then MSB = 0, else MSB = 1
                  (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t5 x0 (intXLEN 1)))
                  (sub t6 t4 t6)
                  (~>>when _ (λ(mem) (not (bveq (read-register t6 mem) (intXLEN 0)))) (addi t6 x0 (intXLEN 1)))
                  (add t5 t5 t6)
                  ;; sum of MSBs has to be exactly 2 (1&1)
                  (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t5 mem))) (addi s1 s1 (intXLEN 1)))
                  (addi t1 t1 (intXLEN 1))		;; i++
                  (slli t3 t3 (intXLEN 1))
                  (slli t4 t4 (intXLEN 1)))
               (sw s1 (intXLEN 36) sp)
               (lw s1 (intXLEN 28) sp)
               (lw t2 (intXLEN 24) sp)
               (lw t1 (intXLEN 20) sp)
               (lw t0 (intXLEN 16) sp)
               (lw t6 (intXLEN 12) sp)
               (lw t5 (intXLEN 8) sp)
               (lw t4 (intXLEN 4) sp)
               (lw t3 (intXLEN 0) sp)
               (lw rd (intXLEN 36) sp)
               (addi sp sp (intXLEN 40))
               (set-pc start-pc)
               (increment-pc))))))

(define (mysll rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -16))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (sw rs2 (intXLEN 12) sp)
     (lw t1 (intXLEN 8) sp)
     (lw t0 (intXLEN 12) sp)
     (andi t0 t0 (intXLEN 31))				;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (sll rd t1 t0))
     (sw rd (intXLEN 12) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 12) sp)
     (addi sp sp (intXLEN 16))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;; Shift left by 1 is the same as * 2
;; The "safe" shift versions assume that the value of rs2 is valid
(define (mysll-safe rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -24))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter i
     (sw t3 (intXLEN 12) sp)
     (sw rs1 (intXLEN 16) sp)
     (sw rs2 (intXLEN 20) sp)
     (lw t3 (intXLEN 16) sp)				;; t3 = rs1, will become rd
     (lw t1 (intXLEN 20) sp)				;; loop bound = rs2
     (sub t2 x0 x0)
     (sub t0 x0 t3)					;; t0 = -rd
     ;; for rs2 times do rd-(-rd)
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (sub t3 t3 t0)
        (sub t0 x0 t3)
        (addi t2 t2 (intXLEN 1)))			;; i++
     (sw t3 (intXLEN 20) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 20) sp)
     (addi sp sp (intXLEN 24))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysrl rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -16))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (sw rs2 (intXLEN 12) sp)
     (lw t1 (intXLEN 8) sp)
     (lw t0 (intXLEN 12) sp)
     (andi t0 t0 (intXLEN 31))				;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (srl rd t1 t0))
     (sw rd (intXLEN 12) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 12) sp)
     (addi sp sp (intXLEN 16))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

; Shift right by 1 is the same as a circular shift left by 31 (32-1)
(define (mysrl-safe rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -32))
     (sw t0 (intXLEN 0) sp)				;; shifting rs1
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter
     (sw t3 (intXLEN 12) sp)				;; will become rd
     (sw t4 (intXLEN 16) sp)				;; MSB eliminated rs1
     (sw t5 (intXLEN 20) sp)				;; MSB of rs1
     (sw rs1 (intXLEN 24) sp)
     (sw rs2 (intXLEN 28) sp)
     (lw t0 (intXLEN 24) sp)
     (lw t4 (intXLEN 24) sp)
     (lw t2 (intXLEN 28) sp)
     (addi t1 x0 (intXLEN XLEN))
     (sub t1 t1 t2)					;; loop bound = 32-rs2
     (sub t2 x0 x0)
     (sub t3 x0 x0)
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t4 t4 (intXLEN 1))			;; eliminate MSB
        (srli t4 t4 (intXLEN 1))
        (slli t3 t3 (intXLEN 1))			;; shift result
        (sub t5 t0 t4)
        ;; if t5 == 0 then MSB = 0, else MSB  = 1
        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t3 t3 (intXLEN 1)))
        (addi t2 t2 (intXLEN 1))			;; i++
        (slli t0 t0 (intXLEN 1))			;; shift to look at next bit
        (slli t4 t4 (intXLEN 1)))
     (sw t3 (intXLEN 28) sp)
     (lw t5 (intXLEN 20) sp)
     (lw t4 (intXLEN 16) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 28) sp)
     (addi sp sp (intXLEN 32))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysra rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -16))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (sw rs2 (intXLEN 12) sp)
     (lw t1 (intXLEN 8) sp)
     (lw t0 (intXLEN 12) sp)
     (andi t0 t0 (intXLEN 31))				;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (sra rd t1 t0))
     (sw rd (intXLEN 12) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 12) sp)
     (addi sp sp (intXLEN 16))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;; For arithmetic shift, first copy the MSB for rs2 times
;; Then copy rs1 for (32-rs2) times like in srli
(define (mysra-safe rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -32))
     (sw t0 (intXLEN 0) sp)				;; shifting rs1
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter
     (sw t3 (intXLEN 12) sp)				;; will become rd
     (sw t4 (intXLEN 16) sp)				;; MSB eliminated rs1
     (sw t5 (intXLEN 20) sp)				;; MSB of rs1
     (sw rs1 (intXLEN 24) sp)
     (sw rs2 (intXLEN 28) sp)
     (lw t1 (intXLEN 28) sp)				;; first loop bound is rs2
     (lw t0 (intXLEN 24) sp)
     (lw t4 (intXLEN 24) sp)
     (sub t2 x0 x0)
     (sub t3 x0 x0)
     (slli t4 t4 (intXLEN 1))				;; extract MSB
     (srli t4 t4 (intXLEN 1))
     (sub t5 t0 t4)
     (lw t4 (intXLEN 24) sp)
     ;; if t5 == 0 then MSB = 0, else MSB = 1
     (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t5 x0 (intXLEN 1)))
     ;; first loop, copy MSB for rs2 times
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t3 t3 (intXLEN 1))			;; shift result
        (add t3 t3 t5)
        (addi t2 t2 (intXLEN 1)))
     (addi t2 x0 (intXLEN XLEN))
     (sub t1 t2 t1)					;; new loop bound is (32-rs2)
     (sub t2 x0 x0)					;; reset loop counter
     ;; second loop, copy bits of rs1
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t4 t4 (intXLEN 1))
        (srli t4 t4 (intXLEN 1))
        (slli t3 t3 (intXLEN 1))
        (sub t5 t0 t4)
        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t3 t3 (intXLEN 1)))
        (addi t2 t2 (intXLEN 1))
        (slli t0 t0 (intXLEN 1))			;; look at next bit
        (slli t4 t4 (intXLEN 1)))
     (sw t3 (intXLEN 28) sp)
     (lw t5 (intXLEN 20) sp)
     (lw t4 (intXLEN 16) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 28) sp)
     (addi sp sp (intXLEN 32))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;; Set rd to 1 if rs1 < rs2, 0 otherwise
(define (myslt rd rs1 rs2 mem-state)
  (~>> mem-state
     (~>>if-else _ (λ(mem) (bvslt (read-register rs1 mem) (read-register rs2 mem))) (addi rd x0 (intXLEN 1)) (sub rd x0 x0))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysltu rd rs1 rs2 mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -20))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw t2 (intXLEN 8) sp)
     (sw rs1 (intXLEN 12) sp)
     (sw rs2 (intXLEN 16) sp)
     (lw t0 (intXLEN 12) sp)
     (lw t1 (intXLEN 16) sp)
     (srli t0 t0 (intXLEN 1))				;; eliminate LSB
     (srli t1 t1 (intXLEN 1))
     ;; if upper 31 bits are equal, compare lower 31 bits
     (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t1 mem)))
        (lw t0 (intXLEN 12) sp)
        (lw  t1 (intXLEN 16) sp)
        (slli t0 t0 (intXLEN 1))			;; eliminate MSB
        (srli t0 t0 (intXLEN 1))
        (slli t1 t1 (intXLEN 1))
        (srli t1 t1 (intXLEN 1)))
     (slt t2 t0 t1)					;; compare with slt
     (sw t2 (intXLEN 16) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 16) sp)
     (addi sp sp (intXLEN 20))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;============== I-Type
(define (myaddi rd rs1 imm mem-state)
  (~>> mem-state
       (addi sp sp (intXLEN -16))
       (sw t0 (intXLEN 0) sp)
       (sw t1 (intXLEN 4) sp)
       (sw t2 (intXLEN 8) sp)
       (sub t1 x0 rs1)
       (sub t1 x0 t1)					;; t1= rs1
       (addi t0 x0 imm)
       (sub t0 x0 t0)					;; t0 = -imm
       (sub t2 t1 t0)					;; t2 = rs1-(-imm)
       (sw t2 (intXLEN 12) sp)
       (lw t2 (intXLEN 8) sp)
       (lw t1 (intXLEN 4) sp)
       (lw t0 (intXLEN 0) sp)
       (lw rd (intXLEN 12) sp)
       (addi sp sp (intXLEN 16))
       (set-pc (cpu-pc mem-state))
       (increment-pc)))

(define (myxori rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sub t1 x0 rs1)
     (sub t1 x0 t1)					;; t1 = rs1
     (addi t0 x0 imm)					;; t0 = imm
     (rvxor rd t1 t0)					;; call xor with rs1 and imm
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (myori rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sub t1 x0 rs1)
     (sub t1 x0 t1)					;; t1 = rs1
     (addi t0 x0 imm)					;; t0 = imm
     (rvor rd t1 t0)					;; call or with rs1 and imm
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (myandi rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sub t1 x0 rs1)
     (sub t1 x0 t1)					;; t1 = rs1
     (addi t0 x0 imm)					;; t0 = imm
     (rvand rd t1 t0)					;; call and with rs1 and imm
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (myslli rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (lw t1 (intXLEN 8) sp)
     (addi t0 x0 imm)
     (andi t0 t0 (intXLEN 31))				;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (sll rd t1 t0))
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;; Shift left by 1 is the same as * 2
;; The "safe" shift versions assume that the passed imm value is valid
(define (myslli-safe rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -20))
     (sw t0 (intXLEN 0) sp)				;; shifting rs1
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter
     (sw t3 (intXLEN 12) sp)				;; will become rd
     (sw rs1 (intXLEN 16) sp)
     (lw t3 (intXLEN 16) sp)				;; rd = rs1
     (sub t0 x0 t3)					;; t0 = -rd
     (addi t1 x0 imm)					;; loop bound = imm
     (sub t2 x0 x0)
     ;; loop, rd -(-rd) for imm times
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (sub t3 t3 t0)
        (sub t0 x0 t3)
        (addi t2 t2 (intXLEN 1)))			;; i++
     (sw t3 (intXLEN 16) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 16) sp)
     (addi sp sp (intXLEN 20))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysrli rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (lw t1 (intXLEN 8) sp)
     (addi t0 x0 imm)
     (andi t0 t0 (intXLEN 31))				;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (srl rd t1 t0))
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

; Shift right by 1 is the same as a circular shift left by 31 (32-1)
(define (mysrli-safe rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -28))
     (sw t0 (intXLEN 0) sp)				;; shifting rs1
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter
     (sw t3 (intXLEN 12) sp)				;; will become rd
     (sw t4 (intXLEN 16) sp)				;; MSB eliminated rs1
     (sw t5 (intXLEN 20) sp)				;; MSB of rs1
     (sw rs1 (intXLEN 24) sp)
     (addi t2 x0 imm)
     (addi t1 x0 (intXLEN XLEN))
     (sub t1 t1 t2)					;; loop bound = 32 - imm
     (sub t2 x0 x0)
     (lw t0 (intXLEN 24) sp)
     (sub t3 x0 x0)
     (lw t4 (intXLEN 24) sp)
     ;; for (32-imm) times add the MSB of rs1 to rd
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t4 t4 (intXLEN 1))			;; eliminate MSB
        (srli t4 t4 (intXLEN 1))
        (slli t3 t3 (intXLEN 1))			;; shift result
        (sub t5 t0 t4)
        ;; if t5 == 0 then MSB = 0, else MSB = 1
        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t3 t3 (intXLEN 1)))
        (addi t2 t2 (intXLEN 1))			;; i++
        (slli t0 t0 (intXLEN 1))			;; look at next bit
        (slli t4 t4 (intXLEN 1)))
     (sw t3 (intXLEN 24) sp)
     (lw t5 (intXLEN 20) sp)
     (lw t4 (intXLEN 16) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 24) sp)
     (addi sp sp (intXLEN 28))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysrai rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (lw t1 (intXLEN 8) sp)
     (addi t0 x0 imm)
     (andi t0 t0 (intXLEN 31))			;; extract lowest 5 bits as shift amount
     ;; if shift amount == 0 then just return rs1 as result
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (intXLEN 0))) (add rd x0 t1) (sra rd t1 t0))
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;; For arithmetic shift, first copy the MSB for imm times
;; Then copy rs1 for (32-imm) times like in srli
(define (mysrai-safe rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -28))
     (sw t0 (intXLEN 0) sp)				;; shifting rs1
     (sw t1 (intXLEN 4) sp)				;; loop bound
     (sw t2 (intXLEN 8) sp)				;; loop counter
     (sw t3 (intXLEN 12) sp)				;; will become rd
     (sw t4 (intXLEN 16) sp)				;; MSB eliminated rs1
     (sw t5 (intXLEN 20) sp)				;; MSB of rs1
     (sw rs1 (intXLEN 24) sp)
     (addi t1 x0 imm)					;; first loop bound = imm
     (lw t0 (intXLEN 24) sp)
     (lw t4 (intXLEN 24) sp)
     (sub t2 x0 x0)
     (sub t3 x0 x0)
     (slli t4 t4 (intXLEN 1))				;; eliminate MSB
     (srli t4 t4 (intXLEN 1))
     (sub t5 t0 t4)
     (lw t4 (intXLEN 24) sp)
     ;; if t5 == 0 then MSB = 0, else MSB = 1
     (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t5 x0 (intXLEN 1)))
     ;; first loop, add MSB to result for imm times
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t3 t3 (intXLEN 1))
        (add t3 t3 t5)
        (addi t2 t2 (intXLEN 1)))
     (addi t2 x0 (intXLEN XLEN))
     (sub t1 t2 t1)					;; second loop bound = (32 - imm)
     (sub t2 x0 x0)					;; reset loop variable
     ;; second loop, copy MSB of rs1
     (~>>for-break _ LOOP-LIM
        (λ(i mem) (eq? (intXLEN i) (read-register t2 mem)))
        #:break (λ(i mem) (bveq (intXLEN i) (read-register t1 mem)))
        (slli t4 t4 (intXLEN 1))			;; eliminate MSB
        (srli t4 t4 (intXLEN 1))
        (slli t3 t3 (intXLEN 1))			;; shift result
        (sub t5 t0 t4)
        (~>>when _ (λ(mem) (not (bveq (read-register t5 mem) (intXLEN 0)))) (addi t3 t3 (intXLEN 1)))
        (addi t2 t2 (intXLEN 1))
        (slli t0 t0 (intXLEN 1))			;; look at next bit
        (slli t4 t4 (intXLEN 1)))
     (sw t3 (intXLEN 24) sp)
     (lw t5 (intXLEN 20) sp)
     (lw t4 (intXLEN 16) sp)
     (lw t3 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 24) sp)
     (addi sp sp (intXLEN 28))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (myslti rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (lw t1 (intXLEN 8) sp)				;; t1 = rs1
     (addi t0 x0 imm)					;; t0 = imm
     (slt rd t1 t0)					;; compare with slt
     (sw rd (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

(define (mysltiu rd rs1 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -16))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw t2 (intXLEN 8) sp)
     (sw rs1 (intXLEN 12) sp)
     (lw t0 (intXLEN 12) sp)				;; t0 = rs1
     (addi t1 x0 imm)					;; t1 = imm
     (srli t0 t0 (intXLEN 1))			;; eliminate LSB
     (srli t1 t1 (intXLEN 1))
     ;; if upper 31 bits are the same
     (~>>when _ (λ(mem) (bveq (read-register t0 mem) (read-register t1 mem)))
        (lw t0 (intXLEN 12) sp)
        (addi t1 x0 imm)
        (slli t0 t0 (intXLEN 1))			;; eliminate MSB
        (srli t0 t0 (intXLEN 1))
        (slli t1 t1 (intXLEN 1))
        (srli t1 t1 (intXLEN 1)))
     (slt t2 t0 t1)					;; compare with slt
     (sw t2 (intXLEN 12) sp)
     (lw t2 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 12) sp)
     (addi sp sp (intXLEN 16))
     (set-pc (cpu-pc mem-state))
     (increment-pc)))

;============== Jumps and Branching
; If rs1 == rs2 go to next instruction, else jump to offset
(define (mybne rs1 rs2 imm mem-state)
  (if (bveq (read-register rs1 mem-state) (read-register rs2 mem-state))
      (increment-pc mem-state)
      (beq x0 x0 imm mem-state)))

; If rs1 < rs2 go to next instruction, else jump to offset
(define (mybge rs1 rs2 imm mem-state)
  (if (bvslt (read-register rs1 mem-state) (read-register rs2 mem-state))
      (increment-pc mem-state)
      (beq x0 x0 imm mem-state)))

(define (mybltu rs1 rs2 imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -16))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (sw rs1 (intXLEN 8) sp)
     (sw rs2 (intXLEN 12) sp)
     (lw t0 (intXLEN 8) sp)				;; t0 = rs1
     (lw t1 (intXLEN 12) sp)				;; t1 = rs2
     (srli t0 t0 (intXLEN 1))				;; eliminate LSB
     (srli t1 t1 (intXLEN 1))
     ;; compare upper 31 bits
     (~>>if-else _ (λ(mem) (bveq (read-register t0 mem) (read-register t1 mem)))
        ;; if they are equal, compare lower 31 bits
        (~>> _
           (lw t0 (intXLEN 8) sp)
           (lw t1 (intXLEN 12) sp)
           (slli t0 t0 (intXLEN 1))			;; eliminate MSB
           (srli t0 t0 (intXLEN 1))
           (slli t1 t1 (intXLEN 1))
           (srli t1 t1 (intXLEN 1))
           ;; compare lower 31 bits, if rs1 < rs2 jump to offset, else go to next instruction
           (~>>if-else _ (λ(mem) (bvslt (read-register t0 mem) (read-register t1 mem)))
              (~>>  _
                 (lw t1 (intXLEN 4) sp)
                 (lw t0 (intXLEN 0) sp)
                 (addi sp sp (intXLEN 16))
                 (set-pc (cpu-pc mem-state))
                 (beq x0 x0 imm))
              (~>> _
                 (lw t1 (intXLEN 4) sp)
                 (lw t0 (intXLEN 0) sp)
                 (addi sp sp (intXLEN 16))
                 (set-pc (cpu-pc mem-state))
                 (increment-pc))))
        ;; else just compare upper 31 bits, jump to offset if rs1 < rs2
        (~>>if-else _ (λ(mem) (bvslt (read-register t0 mem) (read-register t1 mem)))
           (~>> _
              (lw t1 (intXLEN 4) sp)
              (lw t0 (intXLEN 0) sp)
              (addi sp sp (intXLEN 16))
              (set-pc (cpu-pc mem-state))
              (beq x0 x0 imm))
           (~>>  _
              (lw t1 (intXLEN 4) sp)
              (lw t0 (intXLEN 0) sp)
              (addi sp sp (intXLEN 16))
              (set-pc (cpu-pc mem-state))
              (increment-pc))))))

; If rs1 < rs2 go to next instruction, else jump to offset
(define (mybgeu rs1 rs2 imm mem-state)
  (if (bvult (read-register rs1 mem-state) (read-register rs2 mem-state))
      (increment-pc mem-state)
      (beq x0 x0 imm mem-state)))

(define (myjal rd imm mem-state)
  (~>> mem-state
     (addi sp sp (intXLEN -12))
     (sw t0 (intXLEN 0) sp)
     (sw t1 (intXLEN 4) sp)
     (set-pc (cpu-pc mem-state))			;; reset pc as it was modified
     (auipc t1 (intXLEN 0))				;; t1 = pc
     (addi t0 x0 (intXLEN 4))
     (sub t0 x0 t0)					;; t0 = -4
     (sub t1 t1 t0)					;; pc + 4
     (sw t1 (intXLEN 8) sp)
     (lw t1 (intXLEN 4) sp)
     (lw t0 (intXLEN 0) sp)
     (lw rd (intXLEN 8) sp)
     (addi sp sp (intXLEN 12))
     (set-pc (cpu-pc mem-state))
     (beq x0 x0 imm)))

;========================= Main Instructions
; Matches op-code to respective instructions
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
    [(op-beq rs1 rs2 imm) (beq rs1 rs2 imm mem-state)]
    [(op-bne rs1 rs2 imm) (bne rs1 rs2 imm mem-state)]
    [(op-blt rs1 rs2 imm) (blt rs1 rs2 imm mem-state)]
    [(op-bge rs1 rs2 imm) (bne rs1 rs2 imm mem-state)]
    [(op-bltu rs1 rs2 imm) (blt rs1 rs2 imm mem-state)]
    [(op-bgeu rs1 rs2 imm) (bne rs1 rs2 imm mem-state)]
    [(op-jal rd imm) (jal rd imm mem-state)]
    [(op-jalr rd rs1 imm) (jalr rd rs1 imm mem-state)]
    [(op-lui rd imm) (lui rd imm mem-state)]
    [(op-auipc rd imm) (auipc rd imm mem-state)]
    ; Replaced Instructions
    [(op-myadd rd rs1 rs2) (myadd rd rs1 rs2 mem-state)]
    [(op-myxor rd rs1 rs2) (myxor rd rs1 rs2 mem-state)]
    [(op-myor rd rs1 rs2) (myor rd rs1 rs2 mem-state)]
    [(op-myand rd rs1 rs2) (myand rd rs1 rs2 mem-state)]
    [(op-mysll rd rs1 rs2) (mysll rd rs1 rs2 mem-state)]
    [(op-mysrl rd rs1 rs2) (mysrl rd rs1 rs2 mem-state)]
    [(op-mysra rd rs1 rs2) (mysra rd rs1 rs2 mem-state)]
    [(op-myslt rd rs1 rs2) (myslt rd rs1 rs2 mem-state)]
    [(op-mysltu rd rs1 rs2) (mysltu rd rs1 rs2 mem-state)]
    [(op-myaddi rd rs1 imm) (myaddi rd rs1 imm mem-state)]
    [(op-myxori rd rs1 imm) (myxori rd rs1 imm mem-state)]
    [(op-myori rd rs1 imm) (myori rd rs1 imm mem-state)]
    [(op-myandi rd rs1 imm) (myandi rd rs1 imm mem-state)]
    [(op-myslli rd rs1 imm) (myslli rd rs1 imm mem-state)]
    [(op-mysrli rd rs1 imm) (mysrli rd rs1 imm mem-state)]
    [(op-mysrai rd rs1 imm) (mysrai rd rs1 imm mem-state)]
    [(op-myslti rd rs1 imm) (myslti rd rs1 imm mem-state)]
    [(op-mysltiu rd rs1 imm) (mysltiu rd rs1 imm mem-state)]
    [(op-mybne rs1 rs2 imm) (mybne rs1 rs2 imm mem-state)]
    [(op-mybge rs1 rs2 imm) (mybge rs1 rs2 imm mem-state)]
    [(op-mybltu rs1 rs2 imm) (mybltu rs1 rs2 imm mem-state)]
    [(op-mybgeu rs1 rs2 imm) (mybgeu rs1 rs2 imm mem-state)]
    [(op-myjal rd imm) (myjal rd imm mem-state)]))

; Fetches current instruction from the program list and executes it, runs until last intruction reached or out of fuel
(define-bounded (execute-program instructions mem-state)
  (let ([pc (cpu-pc mem-state)] [len (length-bv instructions (bitvector XLEN))])
    (cond
      [(bvslt (bvlshr pc (intXLEN 2)) len)
       (execute-program instructions (execute-instruction (list-ref-bv instructions (bvlshr pc (intXLEN 2))) mem-state))]
      [else mem-state])))

; Compares if two memory states are equivalent (PC, Register values, Stack values)
(define (eq-mem-state memory1 memory2)
  (and
   (equal? (cpu-pc memory1) (cpu-pc memory2))
   (equal? (cpu-registers memory1) (cpu-registers memory2))
   (equal?
    (take-bv (cpu-stack memory1) (bvlshr (bvneg (read-register sp memory1)) (intXLEN 2)))
    (take-bv (cpu-stack memory2) (bvlshr (bvneg (read-register sp memory2)) (intXLEN 2))))))