#!/usr/bin/env guile
!#
;; Taking a filename in as an argument, it reads in a file and then finds the 
;; largest product of 5 consecutive digits
;; Jeremy Steward
;; 2013-11-09
(import (srfi srfi-1))

;; Reads a file into a list, using the function func to act as a 'read'
;; function for the data. 
;; This function takes one argument 'func' and returns a function that takes 
;; as an argument an input port which it will then use func to read into a 
;; list. 
(define (read-into-list func)
  (lambda (input-port)
    (let iter ([f input-port]
               [lst '()])
      (let ([x (func f)])
        (if (eof-object? x)
          (reverse lst)
          (iter f (cons x lst)))))))

;; Uses read-char function to read characters into a list
(define read-chars-into-list
  (read-into-list read-char))

;; Defines a function that will convert a list of characters into a series of numbers
(define (char-list->numbers lst)
  (map string->number 
       (map string 
            (filter (lambda (x) (not (eq? x #\newline))) 
                    lst))))

;; This is a function that returns up to the first n elements of the list lst.
;; If n is greater than the length of lst, then it just returns lst.
;;
;; n must be a number greater than or equal to 0 (i.e. positive number). If n 
;; is a negative number, #f is returned. 
;; lst must be a list, not a pair.
(define (first-n-elements n lst) 
  (if (< n 0) 
    #f
    (let iter ([new-list '()]
               [old-list lst]
               [m 0])
      (if (or (eq? m n)
              (null? old-list))
        (reverse new-list)
        (iter (cons (car old-list) new-list) 
              (cdr old-list)
              (+ m 1))))))

;; Finds the greatest product of n consecutive numbers in a file which lists
;; consecutive numbers. (See 8_input.txt as an example)
(define (greatest-product-of-n-numbers n filename) 
  (let iter ([prod 0]
             [list-of-nums (char-list->numbers
                             (call-with-input-file filename 
                                                   read-chars-into-list))])
    (if (null? (cddddr list-of-nums))
      prod
      (let ([temp (fold * 1 (first-n-elements n list-of-nums))])
        (if  (< prod temp)
          (iter temp (cdr list-of-nums))
          (iter prod (cdr list-of-nums)))))))
