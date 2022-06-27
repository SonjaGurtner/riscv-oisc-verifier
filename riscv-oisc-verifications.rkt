#lang rosette

(require "riscv-oisc-interpreter.rkt")

; Define symbolic constants for the memory which will be replaced with values by the SMT Solver (PC, Registers, Stack)
(define-symbolic test-pc intXLEN?)
(define-symbolic test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31 intXLEN?)
(define-symbolic test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31 test-mem32 test-mem33 test-mem34 test-mem35 intXLEN?)

(define test1-mem
  (cpu
   test-pc
   (list (intXLEN 0) test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31)
   (list test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31 test-mem32 test-mem33 test-mem34 test-mem35)))

(define-symbolic test2-pc intXLEN?)
(define-symbolic test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31 intXLEN?)
(define-symbolic test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31 test2-mem32 test2-mem33 test2-mem34 test2-mem35 intXLEN?)

(define test2-mem
  (cpu
   test2-pc
   (list (intXLEN 0) test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31)
   (list test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31 test2-mem32 test2-mem33 test2-mem34 test2-mem35 )))

; Parameters for the verifications
(define-symbolic imm intXLEN?)
(define-symbolic rd rs1 rs2 intXLEN?)

; Not all registers are valid as source or destination registers (i.e. x0 cannot be modified)
; Exclude sp as the macros modify the stack and would therefore lead to inconsistent behaviour
(define (valid-src-reg reg)
  (and (bvsge reg (intXLEN 0)) (not (bveq reg sp)) (bvsle reg (intXLEN 31))))

(define (valid-dest-reg reg)
  (and (bvsge reg (intXLEN 1)) (not (bveq reg sp)) (bvsle reg (intXLEN 31))))

; Verify that executing a RISC-V Instruction and macro Instruction produce equivalent architectural states
; We assume that the stack is big enough, and that the stack pointer is valid (<= 0,  != 1000 in XLEN) as this would be inconsistent in bvneg
; The PC and SP have to be 2-bit aligned (dividable by 4), and the register parameters have to be valid
; Memory1 and Memory2 have to be equal before the execution starts
(define (verify-func-r func1 func2 space)
  (verify
   (begin
     (assume
      (and
       (bvslt (convert-sp-index sp (intXLEN 0) test1-mem) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       (bvsge (read-register sp test1-mem) (bvadd (bvshl (intXLEN 1) (intXLEN (- XLEN 1))) (intXLEN 1)))
       (bvsle (read-register sp test1-mem) (intXLEN 0))
       (bveq (bvsmod (read-register sp test1-mem) (intXLEN 4)) (intXLEN 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (intXLEN 4)) (intXLEN 0))
       (valid-src-reg rs1)
       (valid-src-reg rs2)
       (valid-dest-reg rd)
       (eq-mem-state test1-mem test2-mem)))
     (assert (eq-mem-state
              (func1 rd rs1 rs2 test1-mem)
              (func2 rd rs1 rs2 test2-mem))))))

(define (verify-func-i func1 func2 space)
  (verify
   (begin
     (assume
      (and
       (bvslt (convert-sp-index sp (intXLEN 0) test1-mem) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       (bvsge (read-register sp test1-mem) (bvadd (bvshl (intXLEN 1) (intXLEN (- XLEN 1))) (intXLEN 1)))
       (bvsle (read-register sp test1-mem) (intXLEN 0))
       (bveq (bvsmod (read-register sp test1-mem) (intXLEN 4)) (intXLEN 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (intXLEN 4)) (intXLEN 0))
       (valid-src-reg rs1)
       (valid-dest-reg rd)
       (eq-mem-state test1-mem test2-mem)))
     (assert (eq-mem-state
              (func1 rd rs1 imm test1-mem)
              (func2 rd rs1 imm test2-mem))))))

(define (verify-func-j func1 func2 space)
  (verify
   (begin
     (assume
      (and
       (bvslt (convert-sp-index sp (intXLEN 0) test1-mem) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       (bvsge (read-register sp test1-mem) (bvadd (bvshl (intXLEN 1) (intXLEN (- XLEN 1))) (intXLEN 1)))
       (bvsle (read-register sp test1-mem) (intXLEN 0))
       (bveq (bvsmod (read-register sp test1-mem) (intXLEN 4)) (intXLEN 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (intXLEN 4)) (intXLEN 0))
       (valid-dest-reg rd)
       (eq-mem-state test1-mem test2-mem)))
     (assert (eq-mem-state
              (func1 rd imm test1-mem)
              (func2 rd imm test2-mem))))))

; #:name Name of the instruction
; #:func1, func2 The two functions which are verified to be equal
; #:space The required space on the stack
; #:type To define if the instructions need registers or immediate values as parameters
; #:assumptions Assumptions about the input to define valid inputs
; Main macro which is responsible for the verification, evaluating found counter-examples, and timing information (which is also written to a separate file)
; In case that the SMT solver found a model which is partial, it will be extended to a full model (cex)
(define-syntax-rule (verify-eq #:name name #:func1 func1 #:func2 func2 #:space-on-stack space #:type type #:assumptions assumptions)
  (let ([my-file (open-output-file #:exists 'append #:mode 'text (format "/home/sonja/GitHub/riscv-oisc-verifier/benchmarks/~a.dat" name))])
    (displayln (format "Verifying equality of ~a: ~a and ~a" name func1 func2))
    (define-values (m milli real-milli cpu-time)
      (time-apply (λ () (begin (assume (assumptions test1-mem)) (if (eq? type 'r) (verify-func-r func1 func2 space) (if (eq? type 'i) (verify-func-i func1 func2 space) (verify-func-j func1 func2 space))))) '()))
    (if (eq? (unsat) (first m))
        (begin
          (displayln (format "PASSED in ~a ms" real-milli))
          (fprintf my-file "(~a, ~a)\n" XLEN real-milli))
        (let
            ([cex (complete-solution (first m)
                  (list rs1 rs2 rd
                        test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31
                        test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31
                        test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31
                        test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31))])
            (displayln (format "Failed in ~a ms: ~a\n" real-milli (first m)))
          (displayln (format "Arguments ~a" (evaluate (list (bitvector->integer rd) (bitvector->integer rs1) (bitvector->integer (if (eq? type 'r) rs2 imm))) cex)))
          (displayln "Memory before Execution")
          (print-all-memory (evaluate test1-mem cex))
          (let
              ([mem1-after (evaluate (if (eq? type 'r)(func1 rd rs1 rs2 test1-mem) (if (eq? type 'i) (func1 rd rs1 imm test1-mem) (func1 rd imm test1-mem))) cex)]
               [mem2-after (evaluate (if (eq? type 'r) (func2 rd rs1 rs2 test2-mem) (if (eq? type 'i) (func2 rd rs1 imm test2-mem) (func2 rd imm test2-mem))) cex)])
              (displayln (format "\nMemory after Execution of ~a and ~a:" func1 func2))
            (print-all-memory-many (evaluate mem1-after cex) (evaluate mem2-after cex)))))
    (close-output-port my-file)))

; Compare different solvers
(require rosette/solver/smt/boolector)
(require rosette/solver/smt/z3)
(require rosette/solver/smt/cvc4)
(current-solver (z3))

(display (format "===== Verify equality of Instructions (bit length: ~a)\n" XLEN))

(displayln "Verification of R-Type")

(verify-eq
 #:name "ADD"
 #:func1 add
 #:func2 myadd
 #:space-on-stack (intXLEN 5)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "OR"
 #:func1 rvor
 #:func2 myor
 #:space-on-stack (intXLEN 10)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "XOR"
 #:func1 rvxor
 #:func2 myxor
 #:space-on-stack (intXLEN 10)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "AND"
 #:func1 rvand
 #:func2 myand
 #:space-on-stack (intXLEN 10)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLL-SAFE"
 #:func1 sll
 #:func2 mysll-safe
 #:space-on-stack (intXLEN 6)
 #:type 'r
 #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (intXLEN 0)) (bvsle (read-register rs2 mem) (intXLEN 31)))))

(verify-eq
 #:name "SLL"
 #:func1 sll
 #:func2 mysll
 #:space-on-stack (intXLEN 4)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SRL-SAFE"
 #:func1 srl
 #:func2 mysrl-safe
 #:space-on-stack (intXLEN 8)
 #:type 'r
 #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (intXLEN 0)) (bvsle (read-register rs2 mem) (intXLEN 31)) (bvsle (read-register rs2 mem) (intXLEN XLEN)))))

(verify-eq
 #:name "SRL"
 #:func1 srl
 #:func2 mysrl
 #:space-on-stack (intXLEN 4)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SRA-SAFE"
 #:func1 sra
 #:func2 mysra-safe
 #:space-on-stack (intXLEN 8)
 #:type 'r
 #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (intXLEN 0)) (bvsle (read-register rs2 mem) (intXLEN 31)) (bvsle (read-register rs2 mem) (intXLEN XLEN)))))

(verify-eq
 #:name "SRA"
 #:func1 sra
 #:func2 mysra
 #:space-on-stack (intXLEN 4)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLT"
 #:func1 slt
 #:func2 myslt
 #:space-on-stack (intXLEN 0)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLTU"
 #:func1 sltu
 #:func2 mysltu
 #:space-on-stack (intXLEN 5)
 #:type 'r
 #:assumptions (λ(mem) #t))

(displayln "\nVerification of I-Type")

(verify-eq
 #:name "ADDI"
 #:func1 addi
 #:func2 myaddi
 #:space-on-stack (intXLEN 4)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "ORI"
 #:func1 ori
 #:func2 myori
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "XORI"
 #:func1 xori
 #:func2 myxori
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "ANDI"
 #:func1 andi
 #:func2 myandi
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLLI-SAFE"
 #:func1 slli
 #:func2 myslli-safe
 #:space-on-stack (intXLEN 5)
 #:type 'i
 #:assumptions (λ(mem) (and (bvsgt imm (intXLEN 0)) (bvsle imm (intXLEN 31)))))

(verify-eq
 #:name "SLLI"
 #:func1 slli
 #:func2 myslli
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SRLI-SAFE"
 #:func1 srli
 #:func2 mysrli-safe
 #:space-on-stack (intXLEN 7)
 #:type 'i
 #:assumptions (λ(mem) (and (bvsgt imm (intXLEN 0)) (bvsle imm (intXLEN 31)) (bvsle imm (intXLEN XLEN)))))

(verify-eq
 #:name "SRLI"
 #:func1 srli
 #:func2 mysrli
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SRAI-SAFE"
 #:func1 srai
 #:func2 mysrai-safe
 #:space-on-stack (intXLEN 7)
 #:type 'i
 #:assumptions (λ(mem) (and (bvsgt imm (intXLEN 0)) (bvsle imm (intXLEN 31)) (bvsle imm (intXLEN XLEN)))))

(verify-eq
 #:name "SRAI"
 #:func1 srai
 #:func2 mysrai
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLTI"
 #:func1 slti
 #:func2 myslti
 #:space-on-stack (intXLEN 3)
 #:type 'i
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLTIU"
 #:func1 sltiu
 #:func2 mysltiu
 #:space-on-stack (intXLEN 4)
 #:type 'i
 #:assumptions (λ(mem) #t))

(displayln "\nVerification of Jumps & Branching")

(verify-eq
 #:name "BNE"
 #:func1 bne
 #:func2 mybne
 #:space-on-stack (intXLEN 0)
 #:type 'i
 #:assumptions (λ(mem) (bveq (bvsmod imm (intXLEN 4)) (intXLEN 0))))

(verify-eq
 #:name "BGE"
 #:func1 bge
 #:func2 mybge
 #:space-on-stack (intXLEN 0)
 #:type 'i
 #:assumptions (λ(mem) (bveq (bvsmod imm (intXLEN 4)) (intXLEN 0))))

(verify-eq
 #:name "BLTU"
 #:func1 bltu
 #:func2 mybltu
 #:space-on-stack (intXLEN 4)
 #:type 'i
 #:assumptions (λ(mem) (bveq (bvsmod imm (intXLEN 4)) (intXLEN 0))))

(verify-eq
 #:name "BGEU"
 #:func1 bgeu
 #:func2 mybgeu
 #:space-on-stack (intXLEN 0)
 #:type 'i
 #:assumptions (λ(mem) (bveq (bvsmod imm (intXLEN 4)) (intXLEN 0))))

(verify-eq
 #:name "JAL"
 #:func1 jal
 #:func2 myjal
 #:space-on-stack (intXLEN 3)
 #:type 'j
 #:assumptions (λ(mem) (bveq (bvsmod imm (intXLEN 4)) (intXLEN 0))))

(displayln "\nVerification of Nested Instructions")

(verify-eq
 #:name "AND"
 #:func1 rvand
 #:func2 myand-nested
 #:space-on-stack (intXLEN 26)
 #:type 'r
 #:assumptions (λ(mem) #t))

(verify-eq
 #:name "SLL"
 #:func1 sll
 #:func2 mysll-nested
 #:space-on-stack (intXLEN 33)
 #:type 'r
 #:assumptions (λ(mem) #t))

