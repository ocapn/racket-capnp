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
         segment-offset-byte-ref
         message-get-segment
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

(: segment-offset-byte-ref (-> segment Integer Integer Integer))
(define (segment-offset-byte-ref seg word-offset byte-offset)
  ;; Get the byte `byte-offset` bytes after the `word-offset`th 64-bit
  ;; word. Useful when doing byte indexing into a larger structure.
  (bytes-ref (segment-bytes seg)
             (+ (nwords->nbytes word-offset) byte-offset)))
  

;;; caps

(struct capability
  ())

;;;; Messages

(struct message
  ([segments : (Mutable-Vectorof segment)]
   [capabilities : (Mutable-Vectorof capability)])
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

(: message-get-segment (-> message Integer segment))
(define (message-get-segment msg i)
  (vector-ref (message-segments msg) i))

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


;;;; io

(: read-bytes-exact (-> Integer Bytes))
(define (read-bytes-exact n)
  ;; like (read-bytes n), but throws an exception on short reads.
  (define bytes (read-bytes n))
  (cond
    ((and (bytes? bytes) (= (bytes-length bytes) 4))
     bytes)
    (else (error "Unexpected EOF"))))

(define (read-u32)
  (parse-u32 (read-bytes-exact 4)))

(: parse-u32 (-> Bytes Integer))
(define (parse-u32 bytes)
  (: get-part (-> Integer Integer))
  (define (get-part n)
    (arithmetic-shift
     (bytes-ref bytes n)
     (nbytes->nbits n)))
  (bitwise-ior
   (get-part 0)
   (get-part 1)
   (get-part 2)
   (get-part 3)))

(: read-message (-> message))
(define (read-message)
  ;; reads a message from (current-input-port). TODO: provide a way to limit
  ;; the size of the message.
  (define segment-count (+ 1 (read-u32)))
  (define segment-table-bytes (read-bytes-exact (* 4 segment-count)))
  (when (= (modulo segment-count 2) 0)
    ; padding
    (read-bytes 4))
  (: parse-segment-length (-> Integer Integer))
  (define (parse-segment-length i)
    (parse-u32 (subbytes segment-table-bytes (* 4 i) (* 4 (+ 1 i)))))
  (define segment-lengths
    (build-vector segment-count parse-segment-length))
  (: read-segment (-> Integer segment))
  (define (read-segment nwords)
    (define nbytes (nwords->nbytes nwords))
    (segment
     (read-bytes-exact nbytes)
     nbytes))
  (define segments
    (vector-map read-segment segment-lengths))
  (message segments (make-vector 0 (capability))))
