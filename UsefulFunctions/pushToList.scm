#lang racket

(define (push x L)
  (cond
    ((null? L) '())
    (else (append (list x (car L)) (cdr L)))
    )
  )

; push 0 to the head of the list
(push 0 '(2 3 4))