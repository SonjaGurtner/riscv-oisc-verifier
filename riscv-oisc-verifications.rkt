#lang rosette

(require "riscv-oisc-interpreter.rkt")

(define-symbolic test-pc int32?)
(define-symbolic test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31 int32?)
(define-symbolic test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31 int32?)

(define test1-mem
  (cpu
   test-pc
   (list (int32 0) test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31)
   (list test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31)))

(define-symbolic test2-pc int32?)
(define-symbolic test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31 int32?)
(define-symbolic test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31 int32?)

(define test2-mem
  (cpu
   test2-pc
   (list (int32 0) test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31)
   (list test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31)))

; parameters for the verifications
(define-symbolic imm int32?)
(define-symbolic rd rs1 rs2 int32?)

(define (valid-src-reg reg)
  (and (bvsge reg (int32 0)) (not (bveq reg sp)) (bvsle reg (int32 31))))

(define (valid-dest-reg reg)
  (and (bvsge reg (int32 1)) (not (bveq reg sp)) (bvsle reg (int32 31))))

;verify that if mem-states are equal, executing a risc-v instruction and replaced instruction produce still equivalent states
;parameters space that instruction will allocate on stack, the two functions that will be verified
(define (verify-func-r func1 func2 space)
  (verify
   (begin
     (assume
      (and
       (bvslt (bvneg (read-register sp test1-mem)) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       ;(bvsge (read-register sp test1-mem) (int32 -10000)) ;; not really useful except for excluding (bv #x80000000 32) whose negation is negative too...
       (bveq (read-register sp test1-mem) (int32 0))
       (bveq (bvsmod (bvneg (read-register sp test1-mem)) (int32 4)) (int32 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (int32 4)) (int32 0))
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
       (bvslt (bvneg (read-register sp test1-mem)) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       ;(bvsge (read-register sp test1-mem) (int32 -10000)) ;; not really useful except for excluding (bv #x80000000 32) whose negation is negative too...
       (bveq (read-register sp test1-mem) (int32 0))
       (bveq (bvsmod (bvneg (read-register sp test1-mem)) (int32 4)) (int32 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (int32 4)) (int32 0))
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
       (bvslt (bvneg (read-register sp test1-mem)) (bvsub (length-bv (cpu-stack test1-mem) (bitvector XLEN)) space))
       ;(bvsge (read-register sp test1-mem) (int32 -10000)) ;; not really useful except for excluding (bv #x80000000 32) whose negation is negative too...
       (bveq (read-register sp test1-mem) (int32 0))
       (bveq (bvsmod (bvneg (read-register sp test1-mem)) (int32 4)) (int32 0))
       (bveq (bvsmod (cpu-pc  test1-mem) (int32 4)) (int32 0))
       (valid-dest-reg rd)
       (eq-mem-state test1-mem test2-mem)))
     (assert (eq-mem-state
              (func1 rd imm test1-mem)
              (func2 rd imm test2-mem))))))

(define-syntax-rule (verify-eq #:func1 func1 #:func2 func2 #:space-on-stack space #:r_type r_type #:i_type i_type #:assumptions assumptions)
  (begin
    (displayln (format "Verifying equality of ~a and ~a" func1 func2))
    (define-values (m milli real-milli cpu-time)
      (time-apply (λ () (begin (assume (assumptions test1-mem)) (if r_type (verify-func-r func1 func2 space) (if i_type (verify-func-i func1 func2 space) (verify-func-j func1 func2 space))))) '()))
    (if (eq? (unsat) (first m))
        (displayln (format "OK ~a ms" real-milli))
        ;; complete the solution in case the model is partial
        (let
            ([cex (complete-solution (first m)
                                     (list rs1 rs2 rd
                                              test-r1 test-r2 test-r3 test-r4 test-r5 test-r6 test-r7 test-r8 test-r9 test-r10 test-r11 test-r12 test-r13 test-r14 test-r15 test-r16 test-r17 test-r18 test-r19 test-r20 test-r21 test-r22 test-r23 test-r24 test-r25 test-r26 test-r27 test-r28 test-r29 test-r30 test-r31
                                              test-mem0 test-mem1 test-mem2 test-mem3 test-mem4 test-mem5 test-mem6 test-mem7 test-mem8 test-mem9 test-mem10 test-mem11 test-mem12 test-mem13 test-mem14 test-mem15 test-mem16 test-mem17 test-mem18 test-mem19 test-mem20 test-mem21 test-mem22 test-mem23 test-mem24 test-mem25 test-mem26 test-mem27 test-mem28 test-mem29 test-mem30 test-mem31
                                              test2-r1 test2-r2 test2-r3 test2-r4 test2-r5 test2-r6 test2-r7 test2-r8 test2-r9 test2-r10 test2-r11 test2-r12 test2-r13 test2-r14 test2-r15 test2-r16 test2-r17 test2-r18 test2-r19 test2-r20 test2-r21 test2-r22 test2-r23 test2-r24 test2-r25 test2-r26 test2-r27 test2-r28 test2-r29 test2-r30 test2-r31
                                              test2-mem0 test2-mem1 test2-mem2 test2-mem3 test2-mem4 test2-mem5 test2-mem6 test2-mem7 test2-mem8 test2-mem9 test2-mem10 test2-mem11 test2-mem12 test2-mem13 test2-mem14 test2-mem15 test2-mem16 test2-mem17 test2-mem18 test2-mem19 test2-mem20 test2-mem21 test2-mem22 test2-mem23 test2-mem24 test2-mem25 test2-mem26 test2-mem27 test2-mem28 test2-mem29 test2-mem30 test2-mem31))])
            (displayln (format "FAIL ~a\n==> ~a ms" (first m) real-milli))
          (displayln (format "Arguments ~a" (evaluate (list (bitvector->integer rd) (bitvector->integer rs1) (bitvector->integer (if r_type rs2 imm))) cex)))
          (displayln "Memory before Execution")
          (for ([k (range 32)] [i (evaluate (cpu-registers test1-mem) cex)])
            (displayln (format "x~a ~a" k i)))
          (let
              ([mem1-after (evaluate (if r_type (func1 rd rs1 rs2 test1-mem) (if i_type (func1 rd rs1 imm test1-mem) (func1 rd imm test1-mem))) cex)]
                [mem2-after (evaluate (if r_type (func2 rd rs1 rs2 test2-mem) (if i_type (func2 rd rs1 imm test2-mem) (func2 rd imm test2-mem))) cex)])
              (displayln (format "PC: ~a \t ~a" (evaluate (cpu-pc mem1-after) cex) (evaluate (cpu-pc mem2-after) cex)))
            (displayln (format "Memory after Execution of ~a and ~a:" func1 func2))
            (for ([k (range 32)] [i (evaluate (cpu-registers mem1-after) cex)] [j (evaluate (cpu-registers mem2-after) cex)])
              (displayln (format "x~a ~a \t2 ~a \t ~a" k i j (if (bveq i j) "" "DIFFERENT"))))
            (displayln "Memory (Stack) after Execution:")
            (for ([k (range 64)] [i (evaluate (cpu-stack mem1-after) cex)] [j (evaluate (cpu-stack mem2-after) cex)])
              (when (< k (bitvector->integer (read-register sp mem1-after)))
                (displayln (format "x~a ~a \t2 ~a \t ~a" k i j (if (bveq i j) "" "DIFFERENT"))))))))))

(display "====Verify equality of instructions\n")
;(require rosette/solver/smt/cvc4)
;(require rosette/solver/smt/boolector)
;;(current-solver (cvc4 #:path "/home/zmaths/.isabelle/contrib/cvc4-1.8/x86_64-linux/cvc4"))
;(current-solver (boolector))
;(output-smt #t)

(displayln "Verification of R-Type")

;; (verify-eq
;;  #:func1 add
;;  #:func2 myadd
;;  #:space-on-stack (int32 5)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 rvor
;;  #:func2 myor
;;  #:space-on-stack (int32 10)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 rvxor
;;  #:func2 myxor
;;  #:space-on-stack (int32 10)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 rvand
;;  #:func2 myand
;;  #:space-on-stack (int32 10)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 sll
;;  #:func2 mysll-safe
;;  #:space-on-stack (int32 6)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (int32 0)) (bvsle (read-register rs2 mem) (int32 31)))))

;; (verify-eq
;;  #:func1 sll
;;  #:func2 mysll
;;  #:space-on-stack (int32 4)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 srl
;;  #:func2 mysrl-safe
;;  #:space-on-stack (int32 8)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (int32 0)) (bvsle (read-register rs2 mem) (int32 31)))))

;; (verify-eq
;;  #:func1 srl
;;  #:func2 mysrl
;;  #:space-on-stack (int32 4)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 sra
;;  #:func2 mysra-safe
;;  #:space-on-stack (int32 8)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) (and (bvsgt (read-register rs2 mem) (int32 0)) (bvsle (read-register rs2 mem) (int32 31)) (bvsle (read-register rs2 mem) (int32 XLEN)))))

;; (verify-eq
;;  #:func1 sra
;;  #:func2 mysra
;;  #:space-on-stack (int32 4)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 slt
;;  #:func2 myslt
;;  #:space-on-stack (int32 0)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 sltu
;;  #:func2 mysltu
;;  #:space-on-stack (int32 5)
;;  #:r_type true
;;  #:i_type false
;;  #:assumptions (λ(mem) #t))

(displayln "\nVerification of I-Type")

;; (verify-eq
;;  #:func1 addi
;;  #:func2 myaddi
;;  #:space-on-stack (int32 4)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 ori
;;  #:func2 myori
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 xori
;;  #:func2 myxori
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 andi
;;  #:func2 myandi
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;; #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 slli
;;  #:func2 myslli-safe
;;  #:space-on-stack (int32 5)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) (and (bvsgt imm (int32 0)) (bvsle imm (int32 31)))))

;; (verify-eq
;;  #:func1 slli
;;  #:func2 myslli
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 srli
;;  #:func2 mysrli-safe
;;  #:space-on-stack (int32 7)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) (and (bvsgt imm (int32 0)) (bvsle imm (int32 31)) (bvsle imm (int32 XLEN)))))

;; (verify-eq
;;  #:func1 srli
;;  #:func2 mysrli
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 srai
;;  #:func2 mysrai-safe
;;  #:space-on-stack (int32 7)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) (and (bvsgt imm (int32 0)) (bvsle imm (int32 31)) (bvsle imm (int32 XLEN)))))

;; (verify-eq
;;  #:func1 srai
;;  #:func2 mysrai
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 slti
;;  #:func2 myslti
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

;; (verify-eq
;;  #:func1 sltiu
;;  #:func2 mysltiu
;;  #:space-on-stack (int32 4)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) #t))

(displayln "\nVerification of Jumps & Branching")

;; (verify-eq
;;  #:func1 bne
;;  #:func2 mybne
;;  #:space-on-stack (int32 0)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) (bveq (bvsmod imm (int32 4)) (int32 0))))

;; (verify-eq
;;  #:func1 bge
;;  #:func2 mybge
;;  #:space-on-stack (int32 0)
;;  #:r_type false
;;  #:i_type true
;;  #:assumptions (λ(mem) (bveq (bvsmod imm (int32 4)) (int32 0))))

;TODO

;; (verify-eq
;;  #:func1 jal
;;  #:func2 myjal
;;  #:space-on-stack (int32 3)
;;  #:r_type false
;;  #:i_type false
;;  #:assumptions (λ(mem) (bveq (bvsmod imm (int32 4)) (int32 0))))
