;; Taking a filename in as an argument, it reads in a file and then finds the 
;; largest product of 5 consecutive digits
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Reads in a file and turns it into a list of characters
(define (read-characters-into-list filePort)
  (define (iter f L) 
	(let ([x (read-char f)])
	  (if (eof-object? x) 
		  L
		  (iter f (append L (list x))))))
  (iter filePort '()))

;; Reads in a file by whitespace and turns it into a list of whatever type is 
;; read in (e.g. a number between whitespace will be a number, while a string will 
;; be read in as a string of characters, and characters will be read in as individual
;; characters. 
;; It is important to note that whitespace (including newline!!!) and the like is
;; completely ignored, unlike the previous function where newline characters are read 
;; in individually as well
(define (read-file-into-list filePort)
  (define (iter f L)
	(let ([x (read f)])
	  (if (eof-object? x)
		  L
		  (iter f (append L (list x))))))
  (iter filePort '()))

;; Defines a function that will convert a list of characters into a series of numbers
(define (list-chars->numbers L)
  (map string->number 
	   (map string 
			(filter (lambda (x) (not (eq? x #\newline))) L))))

;; Finds the greatest product of 5 consecutive numbers in inputFile
(define (greatest-product inputFile)
  (define (select-n-elements-from-list n L)
	(if (or (> n (length L)) (eq? n 0))
	    '()
		(cons (car L) (select-n-elements-from-list (- n 1) (cdr L)))))
  (define (iter prod numList)
	(if (null? (cddddr numList))
	    prod
		(let ([temp (fold * 1 (select-n-elements-from-list 5 numList))])
		  (if (< prod temp)
			  (iter temp (cdr numList))
			  (iter prod (cdr numList))))))
  (iter 0 (list-chars->numbers (call-with-input-file inputFile read-characters-into-list))))
