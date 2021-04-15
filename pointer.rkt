#lang typed/racket

(define-type Ptr Integer)
(define-type PtrKind Integer)

(: ptr-null? (-> Ptr Boolean))
(define (ptr-null? p)
  (equal? 0 p))

(: ptr-kind (-> Ptr PtrKind))
(define (ptr-kind p)
  (bitwise-and p 3))

(define ptr-kind-struct 0)
(define ptr-kind-list 1)
(define ptr-kind-far 2)
(define ptr-kind-cap 3)

(: twos-comp (-> Integer Integer Integer))
(define (twos-comp value size)
  (bitwise-and
    (- (arithmetic-shift 1 size) value)
    (- (arithmetic-shift 1 size) 1)))

(define as-signed twos-comp)

(: struct-or-list-offset (-> Ptr Integer))
(define (struct-or-list-offset p)
  (as-signed (bitwise-bit-field p 2 32) 30))

(define struct-offset struct-or-list-offset)

(: struct-nwords (-> Ptr Integer))
(define (struct-nwords p)
  (bitwise-bit-field p 32 48))

(: struct-nptrs (-> Ptr Integer))
(define (struct-nptrs p)
  (bitwise-bit-field p 48 64))

(: make-struct-ptr (-> Integer Integer Integer Ptr))
(define (make-struct-ptr offset nwords nptrs)
  (bitwise-ior
    (arithmetic-shift (twos-comp offset 30) 2)
    (arithmetic-shift nwords 32)
    (arithmetic-shift nptrs 48)))

(define list-offset struct-or-list-offset)

(: list-c (-> Ptr Integer))
(define (list-c p)
  (bitwise-bit-field p 32 35))

(: list-size (-> Ptr Integer))
(define (list-size p)
  (bitwise-bit-field p 35 64))

(: far-landing-pad-two-words? (-> Ptr Boolean))
(define (far-landing-pad-two-words? p)
  (equal? 1 (bitwise-bit-field p 2 3)))

(: far-offset (-> Ptr Integer))
(define (far-offset p)
  (as-signed (bitwise-bit-field p 3 32) 29))

(: far-segment (-> Ptr Integer))
(define (far-segment p)
  (bitwise-bit-field p 32 64))
