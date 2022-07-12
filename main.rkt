#lang racket

(require "tda-cardset.rkt")

(define cardsSet
  (lambda (EL cardLength maxCards randFunction)
    (if (< maxCards 1)
        (randomDeckFromList EL 6786 (fullSize cardLength (length EL)) cardLength randFunction)
        (randomDeckFromList EL 6786 maxCards cardLength randFunction))))

