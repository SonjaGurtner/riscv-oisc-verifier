#lang rosette

(require "riscv-oisc-interpreter.rkt")

; This file contains  test programs and starting memory (registers, stack)

;  Start memory, can be extended if more space needed
(define test-cpu (cpu (int32 0)
                  (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))
                  (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))))

;Test-Program 1
(define program1 (list (op-addi x4 x0 (int32 4)) (op-addi x5 x0 (int32 9)) (op-bne x4 x5 (int32 8)) (op-addi x10 x0 (int32 88)) (op-addi x10 x10 (int32 1))))
(define memory1 (execute-program program1 test-cpu))

;Test-Program 2
(define program2 (list (op-myaddi x4 x0 (int32 4)) (op-myaddi x5 x0 (int32 9)) (op-mybne x4 x5 (int32 8)) (op-myaddi x10 x0 (int32 88)) (op-myaddi x10 x10 (int32 1))))
(define memory2 (execute-program program2 test-cpu))

; Compare if executing both test programs results in the same architectural state
(displayln "====== Test Memory before execution")
test-cpu
(displayln "\n====== Memory 1 after execution")
memory1
(displayln "\n====== Memory 2 after execution")
memory2
(displayln "\n======Same architectural state?")
(eq-mem-state memory1 memory2)