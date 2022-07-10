#lang racket

(require "tda-element.rkt")

;CONSTRUCTOR

(define initializeElementList
  (lambda (E)
    (list E)))

;PERTENENCIA

(define recEL?
  (lambda (EL)
    (if (null? EL)
        #t
        (if (and (element? (car EL)))
            (recEL? (cdr EL))
            #f))))

(define elementList?
  (lambda (EL)
    (if (null? EL)
        #t
        (recEL? EL))))

;SELECTOR

(define SEBPRec
  (lambda (EL P I)
    (if (= P I)
        (car EL)
        (SEBPRec (cdr EL) P (+ I 1)))))

(define selectElementByPosition
  (lambda (EL P)
    (if (> (length EL) P)
        (SEBPRec EL P 0)
        #f)))

;MODIFICADOR

(define IEIRec
  (lambda (EL E)
    (if (null? EL)
        #f
        (if (equal? (car EL) E)
            #t
            (IEIRec (cdr EL) E)))))

(define isElementIn
  (lambda (EL E)
    (if (and (elementList? EL) (element? E))
        (IEIRec EL E)
         #f)))

(define addElementByValue
  (lambda (EL E)
    (if (and (elementList? EL) (element? E) (not (isElementIn EL E)))
        (cons E EL)
        EL)))

(define DEBVRec
  (lambda (EL E)
    (if (equal? (car EL) E)
        (cdr EL)
        (cons (car EL) (DEBVRec (cdr EL) E)))))

(define deleteElementByValue
  (lambda (EL E)
    (if (and (elementList? EL) (element? E) (isElementIn EL E))
        (DEBVRec EL E)
        EL)))

(define DEBPRec
  (lambda (EL P I)
    (if (= P I)
        (cdr EL)
        (cons (car EL) (DEBPRec (cdr EL) P (+ I 1))))))
    
(define deleteElementByPosition
  (lambda (EL P)
    (if (and (elementList? EL) (> (length EL) P))
        (DEBPRec EL P 0)
        EL)))

;RANDOMIZADOR

(define RELFLRec
  (lambda (EL seed listLength Length Iter newList rF)
    (if (< listLength Length)
        #f
        (if (= Iter Length)
            newList
            (if (isElementIn newList (selectElementByPosition EL (modulo (rF seed) listLength)))
                (RELFLRec EL (* 346 seed) listLength Length Iter newList rF)
                (RELFLRec EL (* 346 seed) listLength Length (+ Iter 1)
                          (cons (selectElementByPosition EL (modulo (rF seed) listLength)) newList) rF))))))

(define randomElementListFromList
  (lambda (EL seed Length rF)
    (RELFLRec EL seed (length EL) Length 0 '() rF)))

(define compareElementLists
  (lambda (EL1 EL2)
    (if (and (null? EL1) (null? EL2))
        #t
        (if (or (null? EL1) (not (elementList? EL1))
                (null? EL2) (not (elementList? EL2)))
            #f
            (if (and (isElementIn EL2 (car EL1)) (not (null? EL2)))
                (compareElementLists (cdr EL1) (deleteElementByValue EL2 (car EL1)))
                #f)))))

(define matchingElement
  (lambda (EL1 EL2)
  (if (and (null? EL1) (null? EL2))
           #f
           (if (or (null? EL1) (not (elementList? EL1))
                   (null? EL2) (not (elementList? EL2)))
               #f
               (if (isElementIn EL2 (car EL1))
                   (getElementContent (car EL1))
                   (matchingElement (cdr EL1) EL2))))))
                        
           
(provide (all-defined-out))