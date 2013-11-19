;; Finds the sum of the even Fibonacci terms that are less than 4 million
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Iterative definition of the Fibonnaci function. Returns the nth Fibonacci
;; number
(define (fib n)
 (define (fib-iter a b n)
  (if (= n 0)
      b
	  (fib-iter (+ a b) a (- n 1))))
 (fib-iter 1 0 n))

;; Generates a list of fibonacci numbers less than n
(define (list-of-fibonacci-numbers n)
  (define (iter flist i)
	(let ([fi (fib i)])
	  (if (> fi n)
		  flist
		  (iter (append flist (list fi)) (+ i 1)))))
  (iter '(0 1) 2))

;; Returns the sum of all fibonacci numbers less than n 
(define (sum-of-even-fibonacci-less-than n)
  (fold + 0 (filter even? (list-of-fibonacci-numbers n))))
