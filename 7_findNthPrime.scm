#!/usr/bin/env guile
!#
;; Finds the 10001st Prime
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Determines if a is evenly divisible by b. If a divides by b (i.e if the 
;; remainder of (/ a b) is zero), then this function will return #t, else 
;; it will return #f
(define (divides-by? a b)
  (eq? (remainder a b) 0))

;; Creates a sieve of erastothenes that is 'x' elements long. This sieve will
;; effectively give you the first 'x' number of primes. 
(define (sieve-of-length x) 
  (let iter ([sieve '(2)]
             [n 3]
             [len-sieve 1])
    (if (eq? len-sieve x) 
      (reverse sieve)
      ;; We try to find the first element that is n is divisible by. If no 
      ;; element is found within the list, then we know n must be a prime
      ;; number.
      (if (find (lambda (x) (divides-by? n x)) 
                sieve)
        (iter sieve (+ n 2) len-sieve)
        (iter (cons n sieve) (+ n 2) (+ len-sieve 1))))))
      
;; Finds the nth prime by creating a sieve of 'n' prime numbers and then 
;; selects the last element of that list (which also happens to be the 
;; first element of the reverse of that list. 
;;
;; This is probably horribly inefficient for what I'm trying to do, but
;; taking the reverse of a list is O(n) and car is O(1) and since this is only
;; performed once it isn't a huge ordeal.
(define (find-nth-prime n)
  (car (reverse (sieve-of-length n))))
