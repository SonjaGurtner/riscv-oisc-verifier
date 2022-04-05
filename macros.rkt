#lang rosette
(require rackunit)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))

; int32 takes as input an integer literal and returns the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; parameters for the verifications
(define-symbolic r1 r2 int32?)

;============== ADD
(define (myadd r1 r2)
  (bvsub r1 (bvsub (int32 0) r2)))

(display "MYADD\n")
(verify (assert (eq? (bvadd r1 r2) (myadd r1 r2))))

;============== RECURSIVE / LOOPS

; Parameter that controls the number of unrollings, for recurive/loop functions
(define fuel (make-parameter 32))

(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Recursion Limit reached")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

;example, can be removed
(define-bounded (bvsqrt n)
  (cond
    [(bvult n (int32 2)) n]
    [else
     (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
     (define s1 (bvadd s0 (int32 1)))
     (if (bvugt (bvmul s1 s1) n) s0 s1)]))

;============== SLL
(define-bounded (mysll-safe r1 r2)
  (cond
    [(bveq r2 (int32 0)) r1]
    [else
     (define rd (bvadd r1 r1))
     (mysll-safe rd (bvsub1 r2))]))

(define (mysll r1 r2)
  (mysll-safe r1 (bvand r2 (int32 31))))
  
(display "MYSLL\n")
;bvshl does not take lower 5 bits as riscv sll does, therefore bvand 31
(verify (assert (eq? (bvshl r1 (bvand r2 (int32 31))) (mysll r1 r2))))
(verify (begin
          (assume (bvsge r2 (int32 0)))
          (assume (bvslt r2 (int32 32)))
          (assert (eq? (bvshl r1 r2) (mysll r1 r2)))))

;============== XOR
(define (myxor r1 r2)
  (let ([x r1])
    (define rd (int32 0))
    (for ([i 32])
      (define ex1 (bvshl (zero-extend (bit i r1) (bitvector 32)) (int32 i)))
      (define ex2 (bvshl (zero-extend (bit i r2) (bitvector 32)) (int32 i)))
      (set! rd (bvadd rd (bvxor ex1 ex2))))
     rd))

(display "MYXOR\n")
(verify (assert (eq? (bvxor r1 r2) (myxor r1 r2))))

;============== AND
(define (myand r1 r2)
  (let ([x r1])
    (define rd (int32 0))
    (for ([i 32])
      (define ex1 (bvshl (zero-extend (bit i r1) (bitvector 32)) (int32 i)))
      (define ex2 (bvshl (zero-extend (bit i r2) (bitvector 32)) (int32 i)))
      (set! rd (bvadd rd (bvand ex1 ex2))))
     rd))

(display "MYAND\n")
(verify (assert (eq? (bvand r1 r2) (myand r1 r2))))


;============== OR
(define (myor r1 r2)
  (let ([x r1])
    (define rd (int32 0))
    (for ([i 32])
      (define ex1 (bvshl (zero-extend (bit i r1) (bitvector 32)) (int32 i)))
      (define ex2 (bvshl (zero-extend (bit i r2) (bitvector 32)) (int32 i)))
      (set! rd (bvadd rd (bvor ex1 ex2))))
     rd))

(display "MYOR\n")
(verify (assert (eq? (bvor r1 r2) (myor r1 r2))))

;============== SLT
;============== SLTU
;============== Branching?