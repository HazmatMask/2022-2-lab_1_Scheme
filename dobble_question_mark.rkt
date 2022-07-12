#lang racket

(require "tda-element_list.rkt")
(require "tda-card.rkt")

(define CMEICRec
  (lambda (EL1 EL2 count)
    (if (null? EL1)
        count
        (if (isElementIn EL2 (car EL1))
            (CMEICRec (cdr EL1) (deleteElementByValue EL2 (car EL1)) (+ 1 count))
            (CMEICRec (cdr EL1) EL2 count)))))

(define countMatchingElementsInCards
  (lambda (C1 C2)
    (CMEICRec (getCardContent C1) (getCardContent C2) 0)))

(define CCODRec
  (lambda (C CS)
    (if (null? CS)
        #t
        (if (> (countMatchingElementsInCards C (car CS)) 1)
            #f
            (CCODRec C (cdr CS))))))

(define checkCardOverDeck
  (lambda (C CS)
    (CCODRec C CS)))

(define CFDRec
  (lambda (CS)
    (if (null? CS)
        #t
        (if (checkCardOverDeck (car CS) (cdr CS))
            (CFDRec (cdr CS))
            #f))))

(provide (all-defined-out))