;; Finds the 10001st Prime
;; Jeremy Steward
;; 2013-11-09

;; Builds Sieve of Eratosthenes with 'x' number of elements. 
;; This will basically find the first x prime numbers
(define (sieve-of-eratosthenes x)
  ;; Determines if the dividend is divisible by any of the numbers in the list of divisors
  ;; Note that number is a number, and divisors is a list of numbers that will be tested as 
  ;; divisors against our 'number'
  (define (divides? number divisors)
	(if (null? divisors)
	    #f
		(if (eq? (remainder number (car divisors)) 0)
		    #t
			(divides? number (cdr divisors)))))
  (define (iter sieve n len-sieve)
	(if (> len-sieve x) 
	    sieve
		(if (divides? n sieve)
		    (iter sieve (+ n 2) len-sieve)
			(iter (append sieve (list n)) (+ n 2) (+ len-sieve 1)))))
  (iter '(2 3) 3 3))

(define (find-nth-prime n)
  (car (reverse (sieve-of-eratosthenes n))))
