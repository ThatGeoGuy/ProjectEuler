#!/usr/bin/env guile
!#
;; Calculates the sum of all primes below some number n
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))
 
;; Determines if a is evenly divisible by b. If a divides by b (i.e if the 
;; remainder of (/ a b) is zero), then this function will return #t, else 
;; it will return #f
(define (divides-by? a b)
  (eq? (remainder a b) 0))

;; Creates a list that contains a sieve of erastothenes which consists of 
;; all prime numbers less than m. 
(define (sieve-for-less-than m)
  (let iter ([sieve '(2)]
             [n 3])
    (if (> n m) 
      (reverse sieve)
      ;; We try to find the first element that is n is divisible by. If no 
      ;; element is found within the list, then we know n must be a prime
      ;; number.
      (if (find (lambda (x) (divides-by? n x))
                sieve)
        (iter sieve (+ n 2))
        (iter (cons n sieve) (+ n 2))))))

;; Finds and sums all of the primes below n
(define (sum-of-primes-below n)
  (fold + 0 (sieve-for-less-than n)))
