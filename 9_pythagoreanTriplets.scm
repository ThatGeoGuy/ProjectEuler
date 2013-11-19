;; Finds a set of pythagorean triplets that sum to some number (1000 in this case)
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Returns a list from 'start' to 'stop' by incrementing with 'step' each time
(define (range start stop step)
  (define (iter i rng) 
	(if (> i stop) 
		rng
		(iter (+ i step) (append rng (list i)))))
  (iter (+ start step) (list start)))

;; Finds the product of a pythagorean triple (a,b,c) that sum to n
(define (find-triplet-prod n)
  (define (iter i j) 
	(let ([k (- n i j)])
	  (cond ((eq? i n) #f)
			((eq? j 0) (iter (+ i 1) (- n i)))
			((eq? (expt k 2) (+ (expt i 2) (expt j 2))) (* i j k))
			(else (iter i (- j 1))))))
  (iter 1 (- n 1)))
