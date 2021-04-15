#lang typed/racket

(require "units.rkt")
(require "message.rkt")

(struct untyped-struct
  ([segment : segment]
   [offset : Integer]
   [ndata-bytes : Integer]
   [nptrs : Integer]
   [message : message]))

(struct untyped-capability
  ([index : Integer]
   [message : message]))

(struct untyped-list
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
  (cond
    ((untyped-list-composite? lst) lst)
    ((untyped-list-ptrs? lst)
       (untyped-list-composite
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
  (define len (untyped-list-length lst))
  (if (or (< i 0) (>= i len))
      (error "Index out of bounds")
      (match (untyped-list-data-ndatabits lst)
        (0 0)
        (1 (error "TODO"))
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
        
