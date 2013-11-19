;; Calculates the sum of all primes below some number n
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

;; Builds Sieve of Eratosthenes for all prime numbers less than 'm'
(define (sieve-of-eratosthenes m)
  (let ([r (range 2 m 1)])
	(define (iter sieve n) 
	  (if (eq? (length sieve) n)
		  sieve
		  (iter (filter (lambda (x) (or (eq? x (list-ref sieve n))
										(not (eq? (remainder x (list-ref sieve n)) 0))))
						sieve)
				(+ n 1))))
	(iter r 0)))
  
;; Finds and sums all of the primes below n
(define (sum-of-primes-below n)
  (fold + 0 (sieve-of-eratosthenes n)))
