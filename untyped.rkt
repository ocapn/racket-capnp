#lang typed/racket

(provide (struct-out untyped-ptr)
         (struct-out untyped-struct)
         (struct-out untyped-capability)
         (struct-out untyped-list)
         (struct-out untyped-list-data)
         (struct-out untyped-list-ptrs)
         (struct-out untyped-list-composite)

         get-struct-byte
         as-untyped-list-composite
         index-data)

(require "units.rkt")
(require "message.rkt")
(require "pointer.rkt")

(struct untyped-ptr
  ([message : message]))

(struct untyped-struct untyped-ptr
  ([segment : segment]
   [offset : Integer]
   [ndata-bytes : Integer]
   [nptrs : Integer]))

(struct untyped-capability untyped-ptr
  ([index : Integer]))

(struct untyped-list untyped-ptr
  ([segment : segment]
   [offset : Integer]
   [length : Integer]))

(struct untyped-list-data untyped-list
  ([ndatabits : Integer]))

(struct untyped-list-ptrs untyped-list ())

(struct untyped-list-composite untyped-list
  ([ndatabytes : Integer]
   [nptrs : Integer]))

(: as-untyped-list-composite (-> untyped-list untyped-list-composite))
(define (as-untyped-list-composite lst)
  ;; Promote a list of data values or pointers to a list of structs.
  ;; Throws an error if the argument is a list of 1-bit values.
  (cond
    ((untyped-list-composite? lst) lst)
    ((untyped-list-ptrs? lst)
       (untyped-list-composite
        (untyped-ptr-message lst)
        (untyped-list-segment lst)
        (untyped-list-offset lst)
        (untyped-list-length lst)
        0
        1))
    ((untyped-list-data? lst)
     (let ([elt-bits (untyped-list-data-ndatabits lst)])
       (if (equal? elt-bits 1)
         (error "Can't convert list of bits to composite list")
         (untyped-list-composite
          (untyped-ptr-message lst)
          (untyped-list-segment lst)
          (untyped-list-offset lst)
          (untyped-list-length lst)
          (nbits->nbytes-floor elt-bits)
          0))))
    (else
     ;; conceptually the above should be exhaustive, but in principle someone
     ;; could pass in an untyped-list (not a subtype of it) or some
     ;; elsewhere-defined subtype. Better error message? Find a way to
     ;; rule this out via the type system?
     (error "Invalid list"))))


(: index-data (-> untyped-list-data Integer Integer))
(define (index-data lst i)
  ;; Get the `i`th element of `lst`
  (define len (untyped-list-length lst))
  (if (or (< i 0) (>= i len))
      (error "Index out of bounds")
      (match (untyped-list-data-ndatabits lst)
        (0 0)
        (n
         (let*
             ([seg (untyped-list-segment lst)]
              [elts-per-word (floor (/ 64 n))]
              [word-idx
               (+ (untyped-list-offset lst)
                  (floor (/ i elts-per-word)))]
              [shift (* n (remainder i elts-per-word))]
              [word (segment-word-ref seg word-idx)])
           (bitwise-and
            (arithmetic-shift word (- shift))
            (- (arithmetic-shift 1 n) 1)))))))


(: get-struct-byte (-> untyped-struct Integer Integer))
(define (get-struct-byte st i)
  (cond
    ((>= i (untyped-struct-ndata-bytes st)) 0)
    ((< i 0) (error "Out of bounds: i < 0"))
    (else
     (segment-offset-byte-ref
      (untyped-struct-segment st)
      (untyped-struct-offset st)
      i))))


(: follow-ptr (-> segment message Integer Ptr untyped-ptr))
(define (follow-ptr seg msg offset ptr)
  ;; Given a pointer located at `offset` in `seg` (in `msg`),
  ;; with numeric value `ptr`, follow the pointer and return
  ;; its referent.
  (define kind (ptr-kind ptr))
  (if (not (equal? kind ptr-kind-far))
      (follow-near-ptr seg msg offset ptr)
      (let*
         ([land-seg-index (far-segment ptr)]
          [land-seg (message-get-segment msg land-seg-index)]
          [land-offset (far-offset ptr)]
          [land-word-1 (segment-word-ref land-seg land-offset)]
          [two-words? (far-landing-pad-two-words? ptr)])
       (if (not two-words?)
           (follow-near-ptr land-seg
                       msg
                       land-offset
                       land-word-1)
           (let*
               ([land-word-2 (segment-word-ref land-seg (+ 1 land-offset))]
                [double-far-seg-index (far-segment land-word-1)]
                [double-far-seg (message-get-segment msg double-far-seg-index)]
                [double-far-offset (far-offset land-word-1)])
             (follow-near-ptr double-far-seg
                         msg
                         double-far-offset
                         land-word-2))))))

(: follow-near-ptr (-> segment message Integer Ptr untyped-ptr))
(define (follow-near-ptr seg msg offset ptr)
  ;; Like follow-ptr, but throws an error for far-pointers, which
  ;; avoids infinite loops.
  (define kind (ptr-kind ptr))
  (cond
    ((equal? kind ptr-kind-struct)
     (untyped-struct
      msg
      seg
      (+ offset (struct-offset ptr))
      (nwords->nbytes (struct-nwords ptr))
      (struct-nptrs ptr)))
    ((equal? kind ptr-kind-cap)
     (untyped-capability msg (cap-index ptr)))
    ((equal? kind ptr-kind-list)
     (let*
         ([c (list-c ptr)]
          [nbits (list-c->nbits (list-c ptr))]
          [new-offset (+ offset (list-offset ptr))]
          [size (list-size ptr)])
       (cond
         (nbits
          (untyped-list-data msg seg new-offset size nbits))
         ((equal? c list-c-ptr)
          (untyped-list-ptrs msg seg new-offset size))
         ((equal? c list-c-composite)
          (error "TODO"))
         (else
          (error "impossible")))))
    ((equal? kind ptr-kind-far)
     (error "Illegally chained far pointers"))
    (else
     (error "impossible"))))