#!/usr/bin/env guile
!#
;; Finds the sum of all the multiples of 3 or 5 below a number
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Returns a list from 'start' to 'stop' by incrementing with 'step' each time
(define* (range start stop #:optional (step 1))
  (unfold 
    (lambda (x) (> x stop))
    (lambda (x) x)
    (lambda (x) (+ x step))
    start))

;; Returns the sum of all numbers up to but not including n that are evenly 
;; divisible by the numbers listed in 'multiples'
(define (sum-of-multiples-less-than n)
  (fold + 0 (filter (lambda (x)
                            (or (eq? (remainder x 3) 0)
                                (eq? (remainder x 5) 0)))
                    (range 0 (- n 1) 1))))
