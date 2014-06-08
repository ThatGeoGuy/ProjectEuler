#!/usr/bin/env guile
!#
;; Finds the difference between the sum of the squares and the square of the 
;; sum of natural numbers from 1 to n
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

;; Calculates the difference between the sum of squares and the square of the 
;; sum of a set of integers between 1 and n
(define (diff-between-sumSquare-and-squareSum n)
  (let ([num-range (range 1 n)])
    (- (expt (fold + 0 num-range) 2)
       (fold + 0 (map (lambda (x) (expt x 2)) num-range)))))
