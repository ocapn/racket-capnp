#lang typed/racket

(provide make-word-vector
         word-vector-ref
         word-vector-set!)

(require "units.rkt")

(define-type WordVector Bytes)

(: make-word-vector (-> Integer WordVector))
(define (make-word-vector k)
  (make-bytes (nwords->nbytes k)))

(: word-vector-ref (-> WordVector Integer Integer))
(define (word-vector-ref vec k)
  (define start (nwords->nbytes k))
  (: get-part (-> Integer Integer))
  (define (get-part n)
    (arithmetic-shift
      (bytes-ref vec (+ n start))
      (nbytes->nbits n)))
  (bitwise-ior
     (get-part 0)
     (get-part 1)
     (get-part 2)
     (get-part 3)
     (get-part 4)
     (get-part 5)
     (get-part 6)
     (get-part 7)))

(: word-vector-set! (-> WordVector Integer Integer Void))
(define (word-vector-set! vec k word)
  (define start (nwords->nbytes k))
  (: set-part! (-> Integer Void))
  (define (set-part! n)
    (bytes-set!
     vec
     (+ start n)
     (bitwise-and
      (arithmetic-shift word (- (nbytes->nbits n)))
      #xff)))
  (set-part! 0)
  (set-part! 1)
  (set-part! 2)
  (set-part! 3)
  (set-part! 4)
  (set-part! 5)
  (set-part! 6)
  (set-part! 7))

(: word-vector-length (-> WordVector Integer))
(define (word-vector-length vec)
  (nbytes->nwords-floor (bytes-length vec)))
