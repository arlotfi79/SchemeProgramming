#lang racket

;--------------------------------------------------------------------------
; @returns: sum of all elements of the given array
(define (sumArray array)
  (cond
    ((null? array) 0)
    (else (+ (car array) (sumArray (cdr array))))
    )
  )

;--------------------------------------------------------------------------
; multiplies the first array's row with the second array's column
; @return: multiply result (array)
(define (mulArrayResult A B)
  (cond
    ((or (null? A) (null? B)) '())
    (else
     (cons (* (car A) (car B)) (mulArrayResult (cdr A) (cdr B))))
    )
  )

;--------------------------------------------------------------------------
; @returns: combined answer of the 'sumArray' and 'mulArrayResult' functions
(define (mulTwo A B)
  (cond
    ((null? B) '())
    (else
     (cons (sumArray (mulArrayResult A (car B))) (mulTwo A (cdr B)))
     ) 
  )
  )

;--------------------------------------------------------------------------
; because we are using row major for both arrays, we should transpose the
; second one for easier multiplication
; @return: transpose of the given array
(define (transpose m)
	(apply map list m)
)

;--------------------------------------------------------------------------
; mul function
; @returns: result of multiplication
(define (mul arr1 arr2)
  (cond
    ((or (null? arr1) (null? arr2)) '())
    (else (cons (mulTwo (car arr1) arr2) (mul (cdr arr1) arr2)))
    )
  )

;--------------------------------------------------------------------------
; main (divides arrays and calls the mul function)
(define (main array)
  (let
      (
       (arr1 (car array))
       (arr2 (transpose (cadr array)))
       )
    ; call the multiply 
    (mul arr1 arr2)
    )
  )

;--------------------------------------------------------------------------
; tests

(display "First example --> ")
(display (main '(
                 ((1 2) (3 4))  ; 1st array
                 ((5 6) (7 8))) ; 2nd array
              ))

(display "\nSecond example --> ")
(display (main '(
                 ((1 2 3) (4 5 6))  ; 1st array
                 ((7 8) (9 10) (11 12))) ; 2nd array
              ))

