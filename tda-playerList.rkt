#lang racket
;CONSTRUCTOR

(require "tda-player.rkt")

(define IPLRec
  (lambda (PL n)
    (if (= (length PL) n)
        PL
        (IPLRec (cons (player "voidname" '()) PL) n))))

(define initializePlayerList
  (lambda (n)
    (IPLRec '() n)))

(define AHPRec
  (lambda (P PL)
    (if (null? PL)
        '()
        (if (equal? "voidname" (getPlayerName (car PL)))
            (cons P (cdr PL))
            (cons (car PL) (AHPRec P (cdr PL)))))))

(define addHumanPlayer
  (lambda (P PL)
    (AHPRec P PL)))

(define deleteHumanPlayerByName
  (lambda (P PL)
    (if (null? PL)
        '()
        (if (equal? (getPlayerName P) (getPlayerName (car PL)))
            (cons (player "voidname" '((1 (1 2 3) (1 2 3) (("element" "null"))))) (cdr PL))
            (cons (car PL) (deleteHumanPlayerByName P (cdr PL)))))))

;PERTENENCIA

(define playerList?recursive
  (lambda (PL)
    (if (null? PL)
        #t
        (if (and (pair? PL)
                 (player? (car PL)))
            (playerList?recursive (cdr PL))
            #f))))

(define playerList?
  (lambda (PL)
    (playerList?recursive PL)))

;SELECTORES

(define GPBNFLRec
  (lambda (PL name)
    (if (null? PL)
        #f
        (if (equal? (getPlayerName (car PL)) name)
            (car PL)
            (GPBNFLRec (cdr PL) name)))))

(define getPlayerByNameFromList
  (lambda (PL name)
    (GPBNFLRec PL name)))

;MODIFICADORES

(define modPlayerFromList
  (lambda (PL P)
    (if (null? PL)
        #f
        (if (equal? (getPlayerName (car PL)) (getPlayerName P))
            (cons P (cdr PL))
            (cons (car PL) (modPlayerFromList (cdr PL) P))))))

(define emptyPlayers?
  (lambda (PL)
    (if (false? (getPlayerByNameFromList PL "voidname"))
        #f
        #t)))

(define getLastPlayer
  (lambda (PL)
    (if (null? (cdr PL))
        (car PL)
        (getLastPlayer (cdr PL)))))

(define getNextPlayer
  (lambda (PL name)
    (if (equal? (getPlayerName (car PL)) name)
        (cadr PL)
        (getNextPlayer (cdr PL) name))))
                 
(provide (all-defined-out))