#lang racket

; function that is passed to reduces must have 2 parameters 
(define (reduce fun L)
  (cond
    ((null? L) 0)
    ((null? (cdr L)) (car L)) ; base case: if it has only has one element then return it
    (else
     (let
         (
          (first_part (fun (car L) (cadr L)))
          (second_part (cddr L))
          )
       (reduce fun (cons first_part second_part))
      )
     )
    )
  )

; for example find the maximum number
(display (reduce (lambda (x y) (y x)) '(1 2 3)))