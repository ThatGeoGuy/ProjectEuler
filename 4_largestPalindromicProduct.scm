#!/usr/bin/env guile
!#
;; Finds the largest palindromic product of n-digit numbers
;; Jeremy Steward
;; 2013-11-09

;; Determines if a number, list, or string is palindromic, that is,
;; whether the exact reversal of the order of symbols (digits in the case
;; of numbers) is the same as the original order of symbols or digits.
(define (palindrome? x)
  (cond 
    [(number? x) 
     (let ([str (number->string x)])
       (equal? (string-reverse str) str))]
    [(list? x) 
     (equal? (reverse x) x)]
    [(string? x) 
     (equal? (string-reverse x) x)]
    [else 
      (error "Unknown type, cannot determine if palindromic: " x)]))

(define (largest-nth-order-palindrome n)
  (let ([max-number (- (expt 10 n) 1)])
    (define (iter i j x) 
      (cond ((<= j 0) (iter (- i 1) (- i 1) x))
            ((<= i (sqrt n)) x)
            ((and (> (* i j) x) (palindrome? (* i j))) (iter i (- j 1) (* i j)))
            (else (iter i (- j 1) x))))
    (if (> n 0)
        (iter max-number max-number 1)
        (error "Cannot determine palindromic numbers if n < 0."))))

(define (largest-nth-order-palindromic-product n) 
  (if (< n 0) 
    (error "Cannot determine palindromic numbers between 1 and 10 ^" n)
    (let ([max-num (- (expt 10 n) 1)])
      (let iter ([i max-num]
                 [j max-num]
                 [x 1])
        (cond 
          [(<= j 0) 
           (iter (- i 1) (- i 1) x)]
          [(<= i (sqrt n))
           x]
          [(and (> (* i j) x)
                (palindrome? (* i j)))
           (iter i (- j 1) (* i j))]
          [else 
            (iter i (- j 1) x)])))))
