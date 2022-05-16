#lang racket

;--------------------------------------------------------------------------
; because we are using row major for both arrays, we should transpose the
; second one for easier multiplication
; @return: transpose of the given array
(define (transpose m)
	(apply map list m)
)

;--------------------------------------------------------------------------
; @returns: sum of all elements of the given array
(define (sumArray array)
  (cond
    ((null? array) 0)
    (else (+ (car array) (sumArray (cdr array))))
    )
  )

;--------------------------------------------------------------------------
(define (rowSum array)
  (cond
    ((null? array) '())
    (else
     (cons (sumArray (car array)) (rowSum (cdr array)))
     )
    )
  )

;--------------------------------------------------------------------------
(define (colSum array)
  (let
      (
       (tArray (transpose array))
       )

    (rowSum tArray)
      )
  )

;--------------------------------------------------------------------------
;tests
(display "Sum of rows --> ")
(display (rowSum '((0 1 2 3 4)(5 6 7 8 9)(0 1 2 3 4)(5 6 7 8 9))))
(display "\nSum of columns --> ")
(display (colSum '((0 1 2 3 4)(5 6 7 8 9)(0 1 2 3 4)(5 6 7 8 9))))

