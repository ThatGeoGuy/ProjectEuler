;; Finds the difference between the sum of the squares and the square of the 
;; sum of natural numbers from 1 to n
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

(define (diff-between-sumSquare-and-squareSum n)
  (let ([num-range (range 1 n 1)])
	(- (expt (fold + 0 num-range) 2)
	   (fold + 0 (map (lambda (x) (expt x 2)) num-range)))))
