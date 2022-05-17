#lang rosette

(require "riscv-oisc-interpreter.rkt")

;######################################### Test-Programs
;cpu memory, can be extended if more space needed
(define test-cpu (cpu (int32 0)
                  (list (int32 0) (int32 1) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))
                  (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))))

;Test-Program 1
(define program1 (list (op-addi x1 x1 (int32 3)) (op-add x3 x1 x1) (op-or x4 x3 x1)))
(define memory1 (execute-program program1 test-cpu))

;Test-Program 2
(define program2 (list (op-myaddi x1 x1 (int32 3)) (op-myadd x3 x1 x1) (op-myor x4 x3 x1)))
(define memory2 (execute-program program2 test-cpu))

;comparing stack only works if program doesn't allocate memory that it doesn't use
(displayln "====== Test Memory before execution")
test-cpu
(displayln "\n====== Memory 1 after execution")
memory1
(displayln "\n====== Memory 2 after execution")
memory2
(displayln "\nSame architectural state?")
(eq-mem-state memory1 memory2)