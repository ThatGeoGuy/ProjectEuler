#!/usr/bin/env guile
!#
;; Finds a set of pythagorean triplets that sum to some number (1000 in this case)
;; Jeremy Steward
;; 2013-11-09

;; Finds the product of a pythagorean triple (a,b,c) that sum to n
(define (find-triplet-prod n)
  (let iter ([i 1]
             [j (- n 1)])
    (let ([k (- n i j)])
      (cond
        [(eq? i n) #f]
        [(eq? j 0)
         (iter (+ i 1) (- n i))]
        [(eq? (expt k 2) (+ (expt i 2) (expt j 2)))
         (* i j k)]
        [else 
          (iter i (- j 1))]))))
