#lang rosette

(require "riscv-oisc-interpreter.rkt")

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

; parameters for the verifications
(define-symbolic imm int32?)
(define-symbolic rd rs1 integer?)

(define (valid-src-reg reg)
  (and (>= reg 0) (not (= reg sp)) (<= reg 31)))

(define (valid-dest-reg reg)
  (and (>= reg 1) (<= reg 31)))

;verify that if mem-states are equal, executing a risc-v instruction and replaced instruction produce still equivalent states
(verify (begin (assume (and
                    (< (- (bitvector->integer (read-register sp test-mem-state))) (- (length (cpu-stack test-mem-state)) 5))
                    (<= (bitvector->integer (read-register sp test-mem-state)) 0)
                    (valid-src-reg rs1)
                    (valid-dest-reg rd)
                    (not (= rd sp))      ;exclude sp because of the comment above
                    (eq-mem-state test-mem-state test2-mem-state)))
               (assert (eq-mem-state (addi rd rs1 imm test-mem-state) (myaddi rd rs1 imm test2-mem-state)))))