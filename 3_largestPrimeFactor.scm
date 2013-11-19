;; Finds the largest prime factor of a number
;; Jeremy Steward
;; 2013-11-09

(define (prime? n)
 (define (smallest-divisor n)
  (define (find-divisor n test-divisor)
   (define (divides? a b) (= (remainder b a) 0))
   (cond ((> (* test-divisor test-divisor) n) n)
		 ((divides? test-divisor n) test-divisor)
		 (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
 (= n (smallest-divisor n)))

(define (largest-prime-factor x)
  (define (iter max-prime n)
	(cond ((> n (sqrt x)) max-prime)
		  ((and (prime? n) (= (remainder x n) 0)) (iter n (+ n 2)))
		  (else (iter max-prime (+ n 2)))))
  (cond ((or (not (integer? x)) (< x 0)) (error "Can only find prime factors of positive integers: " x))
		((< x 2) (error "Cannot find prime factors of numbers less than smallest prime (2): " x))
		(else (iter 2 3))))
