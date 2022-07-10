#lang racket

;CONSTRUCTOR

(define element
  (lambda (X) (list "element" X)))

;PERTENENCIA
(define element?
  (lambda (X)
    (if (and (equal? "element" (car X))
        (string? (cadr X)))        
        #t
        #f)))

;SELECTOR
(define getElementContent
  (lambda (E) (cadr E)))

;OTROS

(define myLengthRec
  (lambda (L Length)
    (if (null? L)
        Length
        (myLengthRec (cdr L) (+ Length 1)))))

(define myLength
  (lambda (L)
    (myLengthRec L 0)))

(provide (all-defined-out))