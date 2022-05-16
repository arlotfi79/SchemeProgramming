#lang racket

(define (modifyRow fun row)
  (cond
    ((null? row) '())
    (else
     (cons (fun (car row)) (modifyRow fun (cdr row)))
     )
   )
  )

(define (zerofill L)
  (cond
    ((null? L) '())
    (else
     (cons (modifyRow (lambda (x) (if (< x 0) 0 x)) (car L)) (zerofill (cdr L)))
     )
    )

  )

(zerofill  '((1 -2 3 -4 5) (10 -20 30 -40 50)))