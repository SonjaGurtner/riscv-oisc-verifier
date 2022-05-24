#lang rosette

(require "riscv-oisc-interpreter.rkt")

;######################################### Test-Programs
;cpu memory, can be extended if more space needed
(define test-cpu (cpu (int32 0)
                  (list (int32 0) (int32 255) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))
                  (list (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0) (int32 0))))

;Test-Program 1
(define program1 (list (op-srli x3 x1 (int32 22))))
(define memory1 (execute-program program1 test-cpu))

;Test-Program 2
;(define program2 (list (op-mysrli x3 x1 (int32 28))))
;(define memory2 (execute-program program2 test-cpu))
(define memory2 (mysrli-safe x3 x1 (int32 22) test-cpu))

;comparing stack only works if program doesn't allocate memory that it doesn't use
(displayln "====== Test Memory before execution")
test-cpu
(displayln "\n====== Memory 1 after execution")
;memory1
(displayln "\n====== Memory 2 after execution")
;memory2
(for ([k (range 32)] [i (cpu-registers memory1)] [j (cpu-registers memory2)])
              (displayln (format "x~a ~a \t2 ~a \t ~a" k i j (if (bveq i j) "" "DIFFERENT"))))
(displayln "\nSame architectural state?")
(eq-mem-state memory1 memory2)