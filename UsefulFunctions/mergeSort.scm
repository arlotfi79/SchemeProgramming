#lang racket

;--------------------------------------------------------------------
; finds len of a list
(define (len L)
  (cond
    ((null? L) 0)
    (else (+ 1 (len (cdr L))))
  )
)

;--------------------------------------------------------------------
; splits list of L in the following order
; 1:n => 1st list
; n:end => 2nd list
(define (split L n)
  (cond
    ; IF
    ((= n 0)
     ; THEN
     (list '() L)
    )
    
    (else
     (let
        ; NOTE: the expression bellow is only accessible within LET
        (
         (LS (split (cdr L) (- n 1)))
        )
     
        (list
         ; 1st list
         (cons (car L) (car LS))
         ; 2nd list
         (cadr LS)
        )
      )
    )
  )
)

;--------------------------------------------------------------------
(define (merge L1 L2)
  (cond
    ((null? L1) L2)
    ((null? L2) L1)
    (else
     (if (<= (car L1) (car L2))
         (cons (car L1) (merge (cdr L1) L2))
         (cons (car L2) (merge L1 (cdr L2)))
     )
    )
  )
)

;--------------------------------------------------------------------
(define (mergeSort L)
  (if (or (null? L) (null? (cdr L)))
      L
      ;else
      (let
          (
           (LS (split L (quotient (len L) 2))) ; kesht used div
          )

        (merge (mergeSort (car LS)) (mergeSort (cadr LS)))

       )
  )
)


;(display (len '(1 2 3)))
;(display (split '(1 2 3 4 5 6 7) 3))
;(display (merge '(1 2 3) '(4 5 6)))
(display (mergeSort '(1 7 2 8 3 4 1 2 8 10)))
