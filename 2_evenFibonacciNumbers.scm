#!/usr/bin/env guile
!#
;; Finds the sum of the even Fibonacci terms that are less than 4 million
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Iterative definition of the Fibonnaci function. Returns the nth Fibonacci
;; number
(define (fib n)
  (let iter ([a 1]
             [b 0]
             [n n])
    (if (eq? n 0)
      b
      (iter (+ a b) a (- n 1)))))

;; Generates a list of fibonacci numbers less than n
(define (list-of-fib-num-less-than n)
  (let iter ([flist '()]
             [i 0])
    (let ([fi (fib i)])
      (if (> fi n)
        (reverse flist)
        (iter (cons fi flist) (+ i 1))))))

;; Returns the sum of all fibonacci numbers less than n 
(define (sum-of-even-fibonacci-less-than n)
  (fold + 0 (filter even? (list-of-fib-num-less-than n))))
