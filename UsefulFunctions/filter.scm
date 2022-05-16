#lang racket

; if structure:
; (if Condition True_statement False_statement)

(define (filter fun L)
  (if (null? L) L
      ; else if
      (if (fun (car L)) ; if the condition is true
          (cons (car L) (filter fun (cdr L))) ; then include (car L)
          ; else
          (filter fun (cdr L)) ; if not do not include it and just do a recursive call
          )
      )
  )


(display (filter (lambda (x) (zero? (modulo x 2))) '(11 22 34 47 52)))