#lang racket

(require "tda-card.rkt")
(require "tda-element.rkt")
(require "tda-element_list.rkt")
(require "dobble_question_mark.rkt")

;CONSTRUCTOR

(define initializeCardSet
  (lambda (C)
    (if (card? C)
        (list C)
        #f)))

;PERTENENCIA

(define cardSet?
  (lambda (CS)
    (if (null? CS)
        #t
        (if (card? (car CS))
            (cardSet? (cdr CS))
            #f))))

;SELECTOR

(define pickCard
  (lambda (CS)
    (car CS)))

(define SCBPRec
  (lambda (EL P I)
    (if (= P I)
        (car EL)
        (SCBPRec (cdr EL) P (+ I 1)))))

(define selectCardByPosition
  (lambda (EL P)
    (if (> (length EL) P)
        (SCBPRec EL P 0)
        #f)))

;MODIFICADOR

(define ICIRec
  (lambda (CL C)
    (if (null? CL)
        #f
        (if (sameCardContents (car CL) C)
            #t
            (ICIRec (cdr CL) C)))))

(define isCardIn
  (lambda (CL C)
    (if (and (cardSet? CL) (card? C))
        (ICIRec CL C)
         #f)))

(define newCardByValue
  (lambda (CL C)
    (if (and (cardSet? CL) (card? C) (not (isCardIn CL C)))
        (cons C CL)
        CL)))

(define RCSFLRec
  (lambda (EL seed listLength size IterSize Length newList rF)
    (if (< size IterSize)
        #f
        (if (= IterSize size)
            newList
            (if (isCardIn newList (createCard 001 '(1 2 3) '(4 5 6)
                                              (randomElementListFromList EL seed Length rF)))
                (RCSFLRec EL (* 347 seed) listLength size IterSize Length newList rF)
                (RCSFLRec EL (* 347 seed) listLength size (+ 1 IterSize) Length
                          (cons (createCard 001 '(1 2 3) '(4 5 6)
                                              (randomElementListFromList EL seed Length rF))
                                newList) rF))))))

(define randomDeckFromList
  (lambda (EL seed size Length rF)
    (if (false? (fullSize Length (length EL)))
        #f
        (if (< (fullSize Length (length EL)) size)
            #f
            (RCSFLRec EL seed (length EL) size 0 Length '() rF)))))

(define myFactorialR
  (lambda (n m)
    (if (= 1 n)
        m
        (myFactorialR (- n 1) (* m n)))))

(define myFactorial
  (lambda (n)
    (if (or (= n 1) (= n 0))
        1
        (myFactorialR n 1))))

(define fullSize
  (lambda (Length listLength)
    (if (> Length listLength)
        #f
        (/ (myFactorial listLength) (* (myFactorial Length) (myFactorial (- listLength Length)))))))

(define myRandom (lambda (xn)
                   (modulo (+ (* 1103515245 xn) 4257) 2147483647)))

;DOBBLE?

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
        (if (= (countMatchingElementsInCards C (car CS)) 1)
            (CCODRec C (cdr CS))
            #f))))

(define countElementInCardSet
  (lambda (E CS count)
    (if (null? CS)
        count
        (if (and (card? (car CS)) (isElementIn (getCardContent (car CS)) E))
            (countElementInCardSet E (cdr CS) (+ 1 count))
            (countElementInCardSet E (cdr CS) count)))))

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

;NUMCARDS

(define CCICSRec
  (lambda (CS n)
    (if (null? CS)
        n
        (CCICSRec (cdr CS) (+ n 1)))))

;NTHCARD

(define GCBPRec
  (lambda (CS P count)
    (if (= P count)
        (car CS)
        (GCBPRec (cdr CS) P (+ count 1)))))

;FINDTOTALCARDS

(define GNEICRec
  (lambda (ELinCard ELGlobal)
    (if (null? ELinCard)
        ELGlobal
        (if (isElementIn ELGlobal (car ELinCard))
            (GNEICRec (cdr ELinCard) ELGlobal)
            (GNEICRec (cdr ELinCard) (cons (car ELinCard) ELGlobal))))))

(define getNewElementsInCard
  (lambda (C EL)
    (GNEICRec (getCardContent C) EL)))

(define GAEICSRec
  (lambda (CS EL)
    (if (null? CS)
        EL
        (GAEICSRec (cdr CS) (getNewElementsInCard (car CS) EL)))))

(define getAllElementsInCardSet
  (lambda (CS)
    (GAEICSRec CS '())))

(define nextCardAmountRec
  (lambda (C I)
    (if (< (myLength (getNewElementsInCard C '())) (+ (* I I) I 1))
        (+ (* I I) I 1)
        (nextCardAmountRec C (+ I 1)))))
         
(define nextCardAmount
  (lambda (C)
    (nextCardAmountRec C 1)))

;MISSINGCARDS

(define DCBCRec
  (lambda (C CS)
    (if (sameCardContents C (car CS))
        (cdr CS)
        (cons (car CS) (DCBCRec C (cdr CS))))))
  
(define deleteCardByContent
  (lambda (C CS)
    (DCBCRec C CS)))

(define ICISRec
  (lambda (C CS)
    (if (null? CS)
        #f
        (if (sameCardContents (car CS) C)
            #t
            (ICISRec C (cdr CS))))))

(define isCardInSet
  (lambda (C CS)
    (ICISRec C CS)))

(define DSFCSRec
  (lambda (sampleSet CS)
    (if (null? sampleSet)
        CS
        (if (isCardInSet (car sampleSet) CS)
            (DSFCSRec (cdr sampleSet) (deleteCardByContent (car sampleSet) CS))
            (DSFCSRec (cdr sampleSet) CS)))))

(define deleteSampleFromCardSet
  (lambda (sampleSet CS)
    (DSFCSRec sampleSet CS)))

(define createFullDeckFromSampleSet
  (lambda (SampleSet)
    (createDobbleDeckFromElementList (getAllElementsInCardSet SampleSet) 5432 myRandom)))

(define deleteSampleFromFullDeck
  (lambda (sampleSet)
    (deleteSampleFromCardSet sampleSet (createFullDeckFromSampleSet sampleSet))))

;CARDSSET->STRING

(define element->string
  (lambda (E)
    (string-append (getElementContent E))))

(define EL->string
  (lambda (EL ELstring)
    (if (null? EL)
        ELstring
        (string-append (element->string (car EL)) " " (EL->string (cdr EL) "")))))

(define card->string
  (lambda (C)
    (string-append ":: " (EL->string (getCardContent C) "") "::\n")))

(define cardset->stringR
  (lambda (CS CSstring)
    (if (null? CS)
        CSstring
        (string-append (card->string (car CS)) (cardset->stringR (cdr CS) CSstring)))))

;CREATEDOBBLESET

(define ICSRec
  (lambda (EL L S CS)
    (if (= S 0)
        '()
        (if (= L 0)
            (cons (createCard 1 '(1 2 3) '(1 2 3) '()) (ICSRec EL L (- S 1) CS))
            (cons (createCard 1 '(1 2 3) '(1 2 3) (initializeElementList (car EL))) (ICSRec EL (- L 1) (- S 1) CS))))))

(define baseCardSet
  (lambda (EL L S)
    (ICSRec EL L S '())))

(define verticalBorderRec
  (lambda (EL CS L Laux)
    (if (null? EL)
        CS
        (if (> L 0)
            (verticalBorderRec (cdr EL)
                            (cons (modCardContent (car CS) (addElementByValue (getCardContent (car CS)) (car EL))) (cdr CS))
                            (- L 1) Laux)
            (cons (car CS) (verticalBorderRec EL (cdr CS)
                                           (- Laux (myLength (getCardContent (cadr CS))))
                  Laux))))))

(define verticalBorder
  (lambda (EL CS L)
    (verticalBorderRec EL CS L L)))

(define getTheDobbleMagic
  (lambda (I J K N)
    (+ N 2 (* (- K 1) N) (modulo (- (+ (* (- I 1) (- K 1)) J) 1) N))))

(define setUpMatrix
  (lambda (EL L S)
    (verticalBorder EL (baseCardSet EL L S) L)))

(define fillUpCardRec
  (lambda (EL I J K N CS)
    (if (> K N)
        CS
        (fillUpCardRec EL I J (+ K 1) N (cons (selectElementByPosition EL (- (getTheDobbleMagic I J K N) 1)) CS)))))

(define fillUpCard
  (lambda (EL I J K N CS)
    (fillUpCardRec EL I J K N (cons (selectElementByPosition EL I) '()))))

(define fillUpCardSetRec
  (lambda (EL I J K N CS)
    (if (> I N)
        CS
        (if (< J N)
            (cons (createCard 1 '(1 2 3) '(1 2 3) (fillUpCard EL I J K N CS)) (fillUpCardSetRec EL I (+ J 1) K N CS))
            (cons (createCard 1 '(1 2 3) '(1 2 3) (fillUpCard EL I J K N CS)) (fillUpCardSetRec EL (+ I 1) 1 1 N CS))))))

(define fillUpCardSet
  (lambda (EL N CS_in)
    (if (null? (getCardContent (car CS_in)))
        (fillUpCardSetRec EL 1 1 1 N '())
        (cons (car CS_in) (fillUpCardSet EL N (cdr CS_in))))))

(define getElementListFromBiggerSample
  (lambda (EL N)
    (if (= N 0)
        '()
        (cons (car EL) (getElementListFromBiggerSample (cdr EL) (- N 1))))))

(define dobbleDeck
  (lambda (EL N)
    (fillUpCardSet EL (- N 1) (setUpMatrix EL N (+ (* N N) N 1)))))

(define primeNumber?Rec
  (lambda (N I)
    (if (< N 2)
        #f
        (if (= I N)
            #t
            (if (= (modulo N I) 0)
                #f
                (primeNumber?Rec N (+ I 1)))))))

(define primeNumber?
  (lambda (N)
    (primeNumber?Rec N 2)))

(define primePower?Rec
  (lambda (N I)
    (if (= N 1)
        #t
        (if (not (integer? N))
            #f
            (primePower?Rec (/ N I) I)))))
            
(define primePower?
  (lambda (N I)
    (if (and (= (modulo N I) 0) (primeNumber? I))
        (primePower?Rec N I)
        (primePower? N (+ 1 I)))))

(define primeRoot?
  (lambda (N I)
    (if (= N I)
        #f
        (if (and (= (modulo N I) 0) (primeNumber? I) (primePower?Rec N I))
            I
            (primeRoot? N (+ I 1))))))

(define closestPrimePower
  (lambda (N I)
    (if (< N (+ (* I I) I 1))
        (+ (* (- I 1) (- I 1)) I)
        (closestPrimePower N (+ I 1)))))

(define closestPrimeRoot
  (lambda (N I)
    (if (< N (+ (* I I) I 1))
        I
        (closestPrimeRoot N (+ I 1)))))
 
(define createDobbleDeckFromElementList
  (lambda (EL seed mRF)
    (dobbleDeck (randomElementListFromList EL seed (closestPrimePower (myLength EL) 1) mRF) (closestPrimeRoot (myLength EL) 1))))

;DRAWCARD

(define drawCard
  (lambda (CS)
    (car CS)))

(define restOfDeck
  (lambda (CS)
    (cdr CS)))

;FUNCIONES SOLICITADAS

(define dobble?
  (lambda (CS)
    (CFDRec CS)))

(define numCards
  (lambda (CS)
    (CCICSRec CS 0)))

(define nthCard
  (lambda (CS N)
    (GCBPRec CS N 0)))

(define findTotalCards
  (lambda (C)
    (nextCardAmount C)))

(define requiredElements
  (lambda (C)
    (nextCardAmount C)))

(define missingCards
  (lambda (sampleSet)
    (deleteSampleFromFullDeck sampleSet)))

(define cardsSet->string
  (lambda (CS)
    (cardset->stringR CS "")))

(provide (all-defined-out))