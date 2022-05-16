#lang racket

(define (map fun a_list)
  (cond
    ((null? a_list) '())
    (else (cons (fun (car a_list))
                (map fun (cdr a_list))))
    )
  )

(map (lambda (num) (* num num num)) '(1 2 3))