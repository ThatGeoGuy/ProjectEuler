#!/usr/bin/env guile
!#
;; Finds the largest prime factor of a number
;; Jeremy Steward
;; 2013-11-09

;; Determines if a is evenly divisible by b. If a divides by b (i.e if the 
;; remainder of (/ a b) is zero), then this function will return #t, else 
;; it will return #f
(define (divides-by? a b)
  (eq? (remainder a b) 0))

;; Finds the smallest divisor of n (discluding 1). This will test numbers 
;; 2, 3, 5, etc. until a divisor is found. If no divisor is found before a
;; number greater-than n is tested, then n is returned. 
(define (smallest-divisor n)
  (let iter ([test-divisor 2])
    (cond
      ;; If the square of the test-divisor is greater than or equal to n, then
      ;; we return n as the greatest common divisor
      ;; Remember: we shouldn't find any "smallest" divisors that are greater
      ;; than the square root of the number itself. 
      [(> (expt test-divisor 2) n) 
       n]
      ;; If n divides by our test-divisor then we return the test-divisor
      [(divides-by? n test-divisor)
       test-divisor]
      ;; Else we iterate by increasing the value of our test divisor, but
      ;; since any number divisible by an even number is also divisible by
      ;; 2, we can just increase our test-divisor by 1 for the first iteration
      ;; (to 3), and then increase our test-divisor by 2 each iteration 
      ;; thereafter
      [else 
        (if (even? test-divisor)
          (iter (+ test-divisor 1))
          (iter (+ test-divisor 2)))])))

;; Determines the primality of the number. If a number is prime, then this 
;; function will return with #t, otherwise it will return #f
(define (prime? n)
  (eq? n (smallest-divisor n)))


;; Finds the largest prime factor of a number x
(define (largest-prime-factor x)
  ;; Bounds check so we don't try to find the largest prime factor of 
  ;; negative numbers, rational numbers, or numbers less than the 
  ;; smallest prime number (2)
  (if (or (not (integer? x)) 
          (< x 2))
    (error "Can only find prime factors of positive integers greater than or equal to 2: " x)
    (let iter ([max-prime 2]
               [n 3])
      (cond
        [(> n (sqrt x)) 
         max-prime]
        [(and (prime? n)
              (divides-by? x n))
         (iter n (+ n 2))]
        [else 
          (iter max-prime (+ n 2))]))))
