# Examples
## Example 1
A binary tree in Scheme language can be defined as below (Note that no node can have only one child):  
   * (leaf integer-value)  
   * (node left-subtree right-subtree)  

Write the function, sum, in a way that returns the sum of all leaf values given a binary tree as input.  
Example test case:  
Input => (display(sum '(node (leaf 1) (node (leaf 2) (leaf 3)))))  
Output => 6

## Example 2
In Scheme language, the 2D array, A<sub>m*n</sub>, is illustrated as below:  
   * ((a<sub>11</sub> a<sub>12</sub> ... a<sub>1n</sub>) (a<sub>21</sub> a<sub>22</sub> ... a<sub>2n</sub>) (a<sub>m1</sub> a<sub>m2</sub> ... a<sub>mn</sub>))

Write a function to get two arrays as input and return their multiplication.  
Example test case:   
Input => (display (multiply '(((1 2) (3 4)) ((5 6) (7 8)))))  
Output => ((19 22) (43 50))

## Example 3
Write a function as depicted below to return the sum of rows and columns of a given array.
   * (rowSum '((a<sub>11</sub> a<sub>12</sub> ... a<sub>1n</sub>) (a<sub>21</sub> a<sub>22</sub> ... a<sub>2n</sub>) (a<sub>m1</sub> a<sub>m2</sub> ... a<sub>mn</sub>))
     )
   * (colSum '((a<sub>11</sub> a<sub>12</sub> ... a<sub>1n</sub>) (a<sub>21</sub> a<sub>22</sub> ... a<sub>2n</sub>) (a<sub>m1</sub> a<sub>m2</sub> ... a<sub>mn</sub>))
     )

Example test case:  
Input => (display (rowSum '((0 1 2 3 4)(5 6 7 8 9)(0 1 2 3 4)(5 6 7 8 9))))  
Output => (10 35 10 35)

## Example 4
Write a function to get an array as input and returns a new array removing its negative elements and setting them to zero. 

Example test case:   
Input => (display (zerofill '((0 1 -2 3 4)(-5 6 7 8 -9)(0 1 2 3 4)(5 -6 -7 8 9))))  
Output => ((0 1 0 3 4)(0 6 7 8 0)(0 1 2 3 4)(5 0 0 8 9))

## Example 5
Write a function to return a slice of a given array as below:  
* (slice '((a<sub>11</sub> a<sub>12</sub> ... a<sub>1n</sub>) (a<sub>21</sub> a<sub>22</sub> ... a<sub>2n</sub>)) x1 y1 x2 y2)  
     Where:
  * 1 <= x1 <= x2 <= m
  * 1 <= y1 <= y2 <= m

Example test case:   
Input => (display (slice '((0 1 2 3 4)(5 6 7 8 9)(0 1 2 3 4)(5 6 7 8 9)) 2 2 3 4))  
Output => ((6 7 8) (1 2 3))