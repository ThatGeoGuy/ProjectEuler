#!/usr/bin/env guile
!#
;; Determines the smallest positive number that is evenly divisible by 
;; all the numbers within some range 1 to n
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Returns a list from 'start' to 'stop' by incrementing with 'step' each time
(define* (range start stop #:optional (step 1))
  (unfold 
    (lambda (x) (> x stop)) ; Stop when
    (lambda (x) x)          ; Build list with value
    (lambda (x) (+ x step)) ; How to move onto the next element
    start))                 ; Where to start (seed value)

;; Finds the smallest postive number evenly divisible by all numbers from 1 to n
;; Minor enhancements: change the range from 2 to n since 1 times anything is 
;; itself. Mind you, this is somewhat banal and it's more clear what I'm doing 
;; if I go from 1 to n instead of 2 to n. 
(define (smallest-multiple-from-less-than n) 
  (fold (lambda (prod factor)
                (* prod (/ factor (gcd prod factor)))) 
        1 
        (range 1 n)))
