;; Finds the largest palindromic product of n-digit numbers
;; Jeremy Steward
;; 2013-11-09

(define (palindrome? x)
  (cond ((number? x) (equal? (string-reverse (number->string x)) (number->string x)))
		((pair? x) (equal? (reverse x) x))
		((string? x) (equal? (string-reverse x) x))
		(else (error "Unknown type, cannot determine if palindromic: " x))))

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
