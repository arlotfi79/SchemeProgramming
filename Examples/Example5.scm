#lang racket

; Note: matrix length starts from 0-(n-1) NOT 1-n

;-----------------------------------------------------
;finding dimentions of the matrix
(define (dimentions L)
  (list (length L) (length (car L)))
  )

;-----------------------Slice X parts------------------------------
; x1 should be passed
(define (slice_x1 L x1)
  (cond
    ((<= x1 0)
     (if (> (length L) 1)
         L ; if len > 1
         (car L)) ; else
     )
    (else (slice_x1 (cdr L) (- x1 1)))
    )
  )

; x2 - x1 should be passed
(define (slice_x2 L x2)
  (cond
    ((<= x2 0) '())
    (else (cons (car L) (slice_x2 (cdr L) (- x2 1))))
    )
  )

; main function to slice x parts
(define (slice_xs L x1 x2)
  (let
      (
       (x2_x1 (+ (- x2 x1) 1)) ; x2 - x1 (should add 1 to it to at least run once in case of same x1 x2)
       (x1_applies_list (slice_x1 L x1)) ; x1 applied to the list
       )
    (slice_x2 x1_applies_list x2_x1) ; now apply x2
      )
  )

;-----------------------Slice Y parts------------------------------
; helper slice_y1
(define (extract_list_from_end L y1)
  (cond
    ((<= y1 0) L)
    (else (extract_list_from_end (cdr L) (- y1 1)))
    )
  )

; helper slice_y2
(define (extract_list_from_start L y2)
  (cond
    ((<= y2 0) '())
    (else (cons (car L) (extract_list_from_start (cdr L) (- y2 1))))
    )
  )

; y1 should be passed 
(define (slice_y1 L y1)
  (cond
    ((null? L) '())
    (else (cons (extract_list_from_end (car L) y1) (slice_y1 (cdr L) y1)))
    )
  )


; y2 - y1 should be passed
(define (slice_y2 L y2)
  (cond
    ((null? L) '())
    (else (cons (extract_list_from_start (car L) y2) (slice_y2 (cdr L) y2)))
    )
  )

; main function to slice x parts
(define (slice_ys L y1 y2)
  (let
      (
       (y2_y1 (+ (- y2 y1) 1)) ; y2 - y1 (should add 1 to it to at least run once in case of same y1 y2)
       (y1_applies_list (slice_y1 L y1)) ; y1 applied to the list
       )
    (slice_y2 y1_applies_list y2_y1) ; now apply y2
      )
  )
;-----------------------MAIN SLICE------------------------------
(define (slice L x1 y1 x2 y2)
  (let
      (
       (x_applied_list (slice_xs L (- x1 1) (- x2 1)))
       )
    (slice_ys x_applied_list (- y1 1) (- y2 1))
      )
  )


;---------------------------TEST-------------------------------------
(display (slice '((0 1 2 3 4)(5 6 7 8 9)(0 1 2 3 4)(5 6 7 8 9)) 2 2 3 4))


