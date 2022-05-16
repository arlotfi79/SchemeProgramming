#lang racket

(define (sum tree)
    (cond 
      ;------------ Base --------------
      ;IF the first one is a leaf
      ((equal? (car tree) 'leaf)
       ;THEN return its digit
       (cadr tree))
        
      ;--------- Recursive ------------
      ;IF the first one is a node
      ((equal? (car tree) 'node)
       ;THEN check the rest of the tree (both left and right children)
       (+ (sum (cadr tree)) (sum (caddr tree)))
       )
    )
)

(display "First example --> ")
(display(sum '(node (leaf 1) (node (leaf 2) (leaf 3)))))
(display "\nSecond example --> ")
(display(sum '(node (node (leaf 2) (leaf 3)) (node (leaf 2) (leaf 3)))))