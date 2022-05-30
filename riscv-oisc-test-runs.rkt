#lang rosette

(require "riscv-oisc-interpreter.rkt")

; This file contains  test programs and starting memory (registers, stack)

; Initial memory, can be extended if more space needed
(define test-cpu (cpu (intXLEN 0)
                      (build-list 32(λ(x) (intXLEN 0)))
                      (build-list 32 (λ(x) (intXLEN 0)))))

; Test-Program 1
(define program1 (list (op-addi x4 x0 (intXLEN 4)) (op-addi x5 x0 (intXLEN 2)) (op-slli x5 x5 (intXLEN 2)) (op-bne x4 x5 (intXLEN 8)) (op-addi x10 x0 (intXLEN 88)) (op-addi x10 x10 (intXLEN 1)) (op-xor x8 x0 x10)))
(define memory1 (execute-program program1 test-cpu))

; Test-Program 2
(define program2 (list (op-myaddi x4 x0 (intXLEN 4)) (op-myaddi x5 x0 (intXLEN 2)) (op-myslli x5 x5 (intXLEN 2)) (op-mybne x4 x5 (intXLEN 8)) (op-myaddi x10 x0 (intXLEN 88)) (op-myaddi x10 x10 (intXLEN 1)) (op-myxor x8 x0 x10)))
(define memory2 (execute-program program2 test-cpu))

; Compare if executing both test programs results in the same architectural state
(displayln "====== Test Memory before execution")
(print-all-memory test-cpu)
(displayln "\n====== Memory 1 after execution")
(print-all-memory memory1)
(displayln "\n====== Memory 2 after execution")
(print-all-memory memory2)
(displayln "\n====== Same architectural state?")
(let ([eq-mem (eq-mem-state memory1 memory2)])
  (displayln eq-mem)
  (when (not eq-mem) (print-all-memory-many memory1 memory2)))
