;; Determines the smallest positive number that is evenly divisible by 
;; all the numbers within some range 1 to n
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

;; Finds the smallest postive number evenly divisible by all numbers from 1 to n
(define (smallest-multiple n) 
  (fold (lambda (prod factor)
		        (* prod (/ factor (gcd prod factor)))) 
		1 
		(range 2 n)))
