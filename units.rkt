#lang typed/racket

(provide nbytes->nwords-floor
         nwords->nbytes
         nbytes->nbits
         nbits->nbytes-floor)

;;;; Unit conversion
(: nbytes->nwords-floor (-> Integer Integer))
(define (nbytes->nwords-floor nbytes)
  (floor (/ nbytes 8)))

(: nwords->nbytes (-> Integer Integer))
(define (nwords->nbytes nwords)
  (* nwords 8))

(: nbytes->nbits (-> Integer Integer))
(define (nbytes->nbits nbytes)
  (* nbytes 8))

(: nbits->nbytes-floor (-> Integer Integer))
(define (nbits->nbytes-floor nbits)
  (floor (/ nbits 8)))