#lang racket

(require "tda-element_list.rkt")
(require "tda-date.rkt")

;CONSTRUCTOR

(define createCard
  (lambda (ID CD MD content)
    (list ID CD MD content)))

;PERTENENCIA

(define card?
  (lambda (C)
    (if (and (not (null? C))
             (integer? (car C))
             (not (null? (cdr C)))
             (date? (cadr C))
             (not (null? (cddr C)))
             (date? (caddr C))
             (not (null? (cdddr C)))
             (elementList? (cadddr C))
             (null? (cddddr C)))
        #t
        #f)))

;SELECTOR

(define getCardID
  (lambda (C)
    (if (card? C)
        (car C)
        #f)))

(define getCardCreationDate
  (lambda (C)
    (if (card? C)
        (cadr C)
        #f)))

(define getCardModDate
  (lambda (C)
    (if (card? C)
        (caddr C)
        #f)))

(define getCardContent
  (lambda (C)
    (if (card? C)
        (cadddr C)
        #f)))

;MODIFICADOR

(define modCardMD
  (lambda (C newMD)
    (createCard (getCardID C)
                (getCardCreationDate C)
                newMD
                (getCardContent C))))

(define modCardContent
  (lambda (C newContent)
    (createCard (getCardID C)
                (getCardCreationDate C)
                (getCardModDate C)
                newContent)))

;OTROS

(define sameCardContents
  (lambda (C1 C2)
    (compareElementLists (getCardContent C1) (getCardContent C2))))

(define matchingElementInCards
  (lambda (C1 C2)
    (matchingElement (getCardContent C1) (getCardContent C2))))

(provide (all-defined-out))