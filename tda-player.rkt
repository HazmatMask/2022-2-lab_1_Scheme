#lang racket

(require "tda-cardset.rkt")

;CONSTRUCTOR

(define player
  (lambda (name CS)
    (list name CS)))

;PERTENENCIA

(define player?
  (lambda (P)
    (if (and (string? (car P))
             (not (null? (cdr P)))
             (cardSet? (cadr P))
             (null? (cddr P)))
        #t
        #f)))

;SELECTORES

(define getPlayerName
  (lambda (P)
    (if (player? P)
        (car P)
        #f)))

(define getPlayerCards
  (lambda (P)
    (if (player? P)
        (cadr P)
        #f)))

(define getPlayerScore
  (lambda (P)
    (length (getPlayerCards P))))

;MODIFICADORES

(define modPlayerCards
  (lambda (P newCards)
    (player (getPlayerName P) newCards)))

(provide (all-defined-out))