#lang typed/racket

(provide (struct-out segment)
         (struct-out message)
         (struct-out capability)
         (struct-out word-ptr)
         segment-length
         set-segment-length!
         segment-capacity
         segment-remaining-capacity
         segment-allocate!
         segment-word-ref
         message-allocate!)
         

(require "units.rkt")
(require "word-vector.rkt")


;;;; Segments

(struct segment
  ([bytes : Bytes]
   [length-in-bytes : Integer])
  #:mutable)

(: segment-length (-> segment Integer))
(define (segment-length seg)
  (nbytes->nwords-floor (segment-length-in-bytes seg)))

(: set-segment-length! (-> segment Integer Void))
(define (set-segment-length! seg len)
  (set-segment-length-in-bytes! seg (nwords->nbytes len)))

(: segment-capacity (-> segment Integer))
(define (segment-capacity seg)
  (nbytes->nwords-floor (bytes-length (segment-bytes seg))))

(: segment-remaining-capacity (-> segment Integer))
(define (segment-remaining-capacity seg)
  (- (segment-capacity seg) (segment-length seg)))

(: segment-allocate! (-> segment Integer (U Integer #f)))
(define (segment-allocate! seg amount)
  (if (< (segment-remaining-capacity seg) amount)
      (error "Not enough space")
      (let ((offset (segment-length seg)))
        (set-segment-length! seg (+ offset amount))
        offset)))

(: segment-word-ref (-> segment Integer Integer))
(define (segment-word-ref seg k)
  (word-vector-ref (segment-bytes seg) k))
  

;;; caps

(struct capability
  ())

;;;; Messages

(struct message
  ([segments : (Vector segment)]
   [capabilities : (Vector capability)])
  #:mutable)

(struct word-ptr
  ([segment : segment]
   [offset : Integer]))

(: message-allocate! (-> message Integer word-ptr))
(define (message-allocate! msg amount)
  (let* ((segment (message-last-segment msg))
         (offset (segment-allocate! segment amount)))
    (if offset
        (word-ptr segment offset)
        (message-allocate-in-new-segment! msg amount))))

(: message-last-segment (-> message segment))
(define (message-last-segment msg)
  (define segments (message-segments msg))
  (define index (- (vector-length segments) 1))
  (vector-ref segments index))

(: message-allocate-in-new-segment! (-> message Integer word-ptr))
(define (message-allocate-in-new-segment! msg amount)
  (let* ([size (min (message-next-natural-segment-size msg))]
         [segment (message-append-new-segment! msg size)])
    (word-ptr segment
              (or (segment-allocate! segment amount)
                  (error "impossible: we should have made the segment big enough")))))

(: message-next-natural-segment-size (-> message Integer))
(define (message-next-natural-segment-size msg)
  (* 2 (segment-length (message-last-segment msg))))

(: message-append-new-segment! (-> message Integer segment))
(define (message-append-new-segment! msg size)
  (define length-in-bytes (nwords->nbytes size))
  (segment (make-shared-bytes length-in-bytes) length-in-bytes))