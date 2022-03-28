#lang rosette
(require rackunit)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))

; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

(define-symbolic r1 r2 int32?)

;==============ADD
(define (myadd r1 r2)
  (bvsub r1 (bvsub (int32 0) r2)))

(verify (assert (eq? (bvadd r1 r2) (myadd r1 r2))))

;==============SLL
(define (mysll r1 r2)
  (let ([x r1])
    (for ([i (in-naturals (bitvector->natural r2))])
      (set! x (bvadd x x)))
  x))

(define (mysll1 r1 r2)
  (let ([x r1])
    (for ([i 32]
      #:when (< i (bitvector->integer r2)))
      (set! x (bvadd x x)))
  x))

(verify (assert (eq? (bvshl r1 r2) (mysll1 r1 r2))))

