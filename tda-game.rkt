#lang racket

(require "tda-playerList.rkt")
(require "tda-player.rkt")
(require "tda-cardset.rkt")
(require "tda-card.rkt")
(require "tda-element.rkt")

;CONSTRUCTOR

(define game
  (lambda (numPlayers cardsSet mode randomFunction)
    (list
     (player "AREA DE JUEGO" '())
     cardsSet
     (initializePlayerList numPlayers)
     "starting"
     mode
     randomFunction
     "voidname")))

;PERTENENCIA

(define game?
  (lambda (G)
    (if (and (pair? G)
             (player? (car G))
             (not (null? (cdr G)))
             (cardSet? (cadr G))
             (dobble? (cadr G))
             (not (null? (cddr G)))
             (playerList? (caddr G))
             (not (null? (cdddr G)))
             (string? (cadddr G))
             (not (null? (cddddr G)))
             (procedure? (car (cddddr G)))
             (not (null? (cdr (cddddr G))))
             (procedure? (cadr (cddddr G)))
             (not (null? (cddr (cddddr G))))
             (string? (caddr (cddddr G)))
             (null? (cdddr (cddddr G))))
        #t
        #f)))

;SELECTORES

(define getGameArea
  (lambda (G)
    (car G)))

(define getGameDeck
  (lambda (G)
    (cadr G)))

(define getGamePlayers
  (lambda (G)
    (caddr G)))

(define getGameStatus
  (lambda (G)
    (cadddr G)))

(define getGameMode
  (lambda (G)
    (car (cddddr G))))

(define getGameTurn
  (lambda (G)
    (caddr (cddddr G))))

;MODIFICADORES

(define modGameArea
  (lambda (G newArea)
    (cons newArea (cdr G))))

(define modGameDeck
  (lambda (G newDeck)
    (cons (car G) (cons newDeck (cddr G)))))

(define modGamePlayers
  (lambda (G newPlayers)
    (cons (car G) (cons (cadr G) (cons newPlayers (cdddr G))))))

(define modGameStatus
  (lambda (G newStatus)
    (cons (car G) (cons (cadr G) (cons (caddr G) (cons newStatus (cddddr G)))))))

(define modGameMode
  (lambda (G newMode)
    (cons (car G) (cons (cadr G) (cons (caddr G) (cons (cadddr G) (cons newMode (cdr (cddddr G)))))))))

(define modGameTurn
  (lambda (G newTurn)
    (cons (car G) (cons (cadr G) (cons (caddr G)
                                       (cons (cadddr G) (cons (car (cddddr G)) (cons (cadr (cddddr G)) (cons newTurn (cdddr (cddddr G)))))))))))

;STACKMODE

;DECK->AREA

(define cardFromDeckToArea
  (lambda (G)
    (modGameDeck
     (modGameArea G (modPlayerCards
                     (getGameArea G)
                     (newCard (getPlayerCards (getGameArea G))
                              (car (getGameDeck G)))))
     (restOfDeck (getGameDeck G)))))

(define NCFDTARec
  (lambda (G n)
    (if (= n 0)
        G
        (NCFDTARec (cardFromDeckToArea G) (- n 1)))))

(define nCardsFromDeckToArea
  (lambda (G n)
    (if (> n (myLength (getGameDeck G)))
        (NCFDTARec G (myLength (getGameDeck G)))
        (NCFDTARec G n))))
  
(define newCard
  (lambda (deck card)
    (cons card deck)))

;AREA->PLAYER

(define cardFromAreaToPlayerByName
  (lambda (G name)
    (modGameArea (modGamePlayers G (modPlayerFromList (getGamePlayers G)
     (modPlayerCards (getPlayerByNameFromList (getGamePlayers G) name)
                    (newCard (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))
                            (car (getPlayerCards (getGameArea G)))))))
                 (modPlayerCards (getGameArea G)
                                 (restOfDeck (getPlayerCards (getGameArea G)))))))

(define NCFATPBNRec
  (lambda (G n name)
    (if (= n 0)
        G
        (NCFATPBNRec (cardFromAreaToPlayerByName G name) (- n 1) name))))

(define nCardsFromAreaToPlayer
  (lambda (G n name)
    (if (> n (myLength (getPlayerCards (getGameArea G))))
        (NCFATPBNRec G (myLength (getPlayerCards (getGameArea G))) name)
        (NCFATPBNRec G n name))))

;RANDOM PLAYER->AREA

(define RCFPTABNRec_card
  (lambda (PL n)
    (if (= n 0)
        (car PL)
        (RCFPTABNRec_card (cdr PL) (- n 1)))))

(define RCFPTABNRec_deck
  (lambda (PL n)
    (if (= n 0)
        (cdr PL)
        (cons (car PL) (RCFPTABNRec_deck (cdr PL) (- n 1))))))

(define randomCardFromPlayerToAreaByName
  (lambda (G name seed rFn)
    (modGamePlayers (modGameArea G (modPlayerCards
                    (getGameArea G)
                    (newCard (getPlayerCards (getGameArea G)) (RCFPTABNRec_card
                                                               (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))
                                                               (modulo (rFn seed)
                                                                       (myLength
                                                                        (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))))))
                   (modPlayerFromList (getGamePlayers G) (modPlayerCards (getPlayerByNameFromList (getGamePlayers G) name)
                                                                         (RCFPTABNRec_deck (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))
                                                                                           (modulo (rFn seed)
                                                                                                   (myLength
                                                                                                    (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))))))))
(define nRCFP->ARec
  (lambda (G name seed n rFn)
    (if (= n 0)
        G
        (nRCFP->ARec (randomCardFromPlayerToAreaByName G name seed rFn) name seed (- n 1) rFn))))

(define nRandomCardsFromPlayerToArea
  (lambda (G name seed n rFn)
    (if (> n (myLength (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))
        (nRCFP->ARec G name seed (myLength (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))) rFn)
        (nRCFP->ARec G name seed n rFn))))

;PLAYER->AREA

(define cardFromPlayerToAreaByName
  (lambda (G name)
    (modGamePlayers (modGameArea G (modPlayerCards (getGameArea G) (newCard (getPlayerCards (getGameArea G))
                          (car (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))))
                   (modPlayerFromList (getGamePlayers G) (modPlayerCards (getPlayerByNameFromList (getGamePlayers G) name)
                                   (restOfDeck (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))))))

(define NCFPTABNRec
  (lambda (G n name)
    (if (= n 0)
        G
        (NCFPTABNRec (cardFromPlayerToAreaByName G name) (- n 1) name))))

(define nCardsFromPlayerToArea
  (lambda (G n name)
    (if (> n (myLength (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))))
        (NCFPTABNRec G (myLength (getPlayerCards (getPlayerByNameFromList (getGamePlayers G) name))) name)
        (NCFPTABNRec G n name))))                                  

;RANDOMSHUFFLE

(define PRCRec
  (lambda (D n)
    (if (= n 0)
        (car D)
        (PRCRec (cdr D) (- n 1)))))

(define RRDRec
  (lambda (D n)
    (if (= n 0)
        (cdr D)
        (cons (car D) (RRDRec (cdr D) (- n 1))))))

(define SDRec
  (lambda (D1 D2 n)
  (if (null? D1)
      D2
      (SDRec
       (RRDRec D1 (modulo (myRandom n) (myLength D1)))
       (cons (PRCRec D1 (modulo (myRandom n) (myLength D1))) D2)
       (* 567 n)))))
       
(define shuffleDeck
  (lambda (D seed)
    (SDRec D '() seed)))

;AREA -> DECK

(define cardFromAreaToDeck
  (lambda (G)
    (modGameArea (modGameDeck G (newCard (getGameDeck G)
                                         (car (getPlayerCards (getGameArea G)))))
                 (modPlayerCards (getGameArea G)
                                 (restOfDeck (getPlayerCards (getGameArea G)))))))

(define NCFATDRec
  (lambda (G n)
    (if (= n 0)
        G
        (NCFATDRec (cardFromAreaToDeck G) (- n 1)))))

(define nCardsFromAreaToDeck
  (lambda (G n)
    (if (> n (myLength (getPlayerCards (getGameArea G))))
        (NCFATDRec G (myLength (getPlayerCards (getGameArea G))))
        (NCFATDRec G n))))

(define returnNCardsToDeckAndShuffle
  (lambda (G n seed)
    (modGameDeck (nCardsFromAreaToDeck G n)
                 (shuffleDeck (getGameDeck (nCardsFromAreaToDeck G n)) seed))))

;GETMATCHINGELEMENTSINAREA

(define getMatchingElementsInArea
  (lambda (G)
    (matchingElementInCards
     (car (getPlayerCards (getGameArea G)))
     (cadr (getPlayerCards (getGameArea G))))))

;FINISH

(define GTSRec
  (lambda (PL maxPlayers maxScore)
    (if (null? PL)
        maxPlayers
        (if (= maxScore (getPlayerScore (car PL)))
            (GTSRec (cdr PL) (cons (car PL) maxPlayers) maxScore)
            (if (< maxScore (getPlayerScore (car PL)))
                (GTSRec (cdr PL) (cons (car PL) '()) (getPlayerScore (car PL)))
                (GTSRec (cdr PL) maxPlayers maxScore))))))

(define getTopScore
  (lambda (PL)
    (GTSRec PL '() 0)))
  
(define isItATie?
  (lambda (PL)
    (if (> (myLength PL) 1)
        #t
        #f)))

(define PL->sRec
  (lambda (PL S)
    (if (null? (cdr PL))
        (string-append (getPlayerName (car PL)) ".")
        (string-append (getPlayerName (car PL)) ", " (PL->sRec (cdr PL) S)))))

(define playerList->string
  (lambda (PL)
    (PL->sRec PL "")))

(define PEGBack
  (lambda (G)
    (modGamePlayers G (getTopScore (getGamePlayers G)))))    

(define protocolEndGame
  (lambda (G)
    (if (isItATie? (getGamePlayers (PEGBack G)))
        (modGameStatus (PEGBack G)
                       (string-append "empate: " (playerList->string (getGamePlayers (PEGBack G)))))
        (modGameStatus (PEGBack G)
                       (string-append "ganador: " (playerList->string (getGamePlayers (PEGBack G))))))))               

;REGISTER

(define freeName?
  (lambda (G name)
    (if (false? (getPlayerByNameFromList (getGamePlayers G) name))
        #t
        #f)))

(define freeSpot?
  (lambda (G)
    (emptyPlayers? (getGamePlayers G))))

(define addPlayerToGame
  (lambda (G name)
    (if (and (freeName? G name) (freeSpot? G))
        (modGamePlayers G (addHumanPlayer (player name '()) (getGamePlayers G)))
        #f)))

;WHOSETURNISIT?

(define assignFirstTurn
  (lambda (G)
    (modGameTurn G (getPlayerName (car (getGamePlayers G))))))

(define assignNextTurn
  (lambda (G)
    (if (equal? (getGameTurn G) (getPlayerName (getLastPlayer (getGamePlayers G))))
        (modGameTurn G (getPlayerName (car (getGamePlayers G))))
        (modGameTurn G (getPlayerName (getNextPlayer (getGamePlayers G) (getGameTurn G)))))))

;PLAY

(define null
  (lambda (G rn)
    (modGameStatus (if (equal? (getGameMode G) stackMode)
        ((getGameMode G) (assignFirstTurn G))
        (if (equal? (getGameMode G) emptyHandStackMode)
            ((getGameMode G) (assignFirstTurn G) rn)
            (if (equal? (getGameMode G) 3)
                #t
                #f))) "in process")))
        

(define CFATDBRec
  (lambda (C D)
    (if (null? D)
        (cons C '())
        (cons (car D) (CFATDBRec C (cdr D))))))

(define cardFromAreaToDeckBottom
  (lambda (G)
    (modGameArea (modGameDeck G (CFATDBRec (drawCard (getPlayerCards (getGameArea G))) (getGameDeck G)))
                 (modPlayerCards (getGameArea G) (restOfDeck (getPlayerCards (getGameArea G)))))))

(define NCFATDBRec
  (lambda (G n)
    (if (= n 0)
        G
        (NCFATDBRec (cardFromAreaToDeckBottom G) (- n 1)))))

(define nCardsFromAreaToDeckBottom
  (lambda (G n)
    (if (> n (myLength (getPlayerCards (getGameArea G))))
        (NCFATDBRec G (myLength (getPlayerCards (getGameArea G))))
        (NCFATDBRec G n))))

(define spotit
  (lambda (E)
    (lambda (G rFn)
      (if (equal? (getGameMode G) stackMode)
          (if (equal? (getElementContent E) (getMatchingElementsInArea G))
              (assignNextTurn (nCardsFromAreaToPlayer G 2 (getGameTurn G)))
              (assignNextTurn G))
          (if (equal? (getGameMode G) emptyHandStackMode)
              (if (equal? (getElementContent E) (getMatchingElementsInArea G))
                  (assignNextTurn (nCardsFromAreaToDeckBottom G 2))
                  (assignNextTurn (nCardsFromAreaToDeckBottom (nCardsFromAreaToPlayer G 1 (getGameTurn G)) 1)))
              #f)))))

(define pass
  (lambda (G)
    (if (equal? stackMode (getGameMode G))
        (assignNextTurn (returnNCardsToDeckAndShuffle G 2 4321))
        (if (equal? + (getGameMode G))
            #t
            (if (equal? - (getGameMode G))
                #t
                #f)))))

(define finish
  (lambda (G)
    (protocolEndGame G)))

;GAME->STRING

(define player->string
  (lambda (P)
    (string-append (getPlayerName P) ":\n" (cardsSet->string (getPlayerCards P)) "\n")))

(define GP->stringRec
  (lambda (GP GPstring)
    (if (null? GP)
        GPstring
        (string-append (playerScore->string (car GP)) "\n" (GP->stringRec (cdr GP) GPstring)))))

(define GP->string
  (lambda (GP)
    (GP->stringRec GP "")))

(define playerScore->string
  (lambda (P)
    (string-append (getPlayerName P) ": " (number->string (getPlayerScore P)) " PUNTOS.")))

(define deckSize->string
  (lambda (D)
    (string-append "DECK: " (number->string (myLength D)) " CARTAS RESTANTES.\n\n")))

(define area->string
  (lambda (A)
    (string-append ":::" (player->string A) "::")))

(define game->stringBack
  (lambda (G)
    (string-append
     (area->string (getGameArea G))
     (deckSize->string (getGameDeck G))
     ":::PUNTAJES ACTUALES:::\n\n"
     (GP->string (getGamePlayers G)))))

;DECK->PLAYER

(define cardFromDeckToPlayer
  (lambda (G name)
    (cardFromAreaToPlayerByName (cardFromDeckToArea G) name)))

(define dealOneToEachRec
  (lambda (G PL)
    (if (null? PL)
        G
        (dealOneToEachRec (cardFromDeckToPlayer G (getPlayerName (car PL))) (cdr PL)))))

(define dealOneToEach
  (lambda (G)
    (dealOneToEachRec G (getGamePlayers G))))

(define DNTERec
  (lambda (G n)
    (if (> 0 n)
        G
        (DNTERec (dealOneToEach G) (- n 1)))))

(define dealNToEach
  (lambda (G n)
    (if (> (* (myLength (getGamePlayers G)) n) (myLength (getGameDeck G)))
        (DNTERec G (- (/ (myLength (getGameDeck G)) (myLength (getGamePlayers G))) 1))
        (DNTERec G (- n 1)))))

;EMPTYHANDSTACKMODE

;(define EHSMInnerLoop

(define emptyHandStackMode
  (lambda (G rFn)
    (if (equal? (getGameStatus G) "starting")
        (nRandomCardsFromPlayerToArea (nCardsFromDeckToArea (dealNToEach G 2) 1) (getGameTurn G) 543 1 rFn)
        (nRandomCardsFromPlayerToArea (nCardsFromDeckToArea G 1) (getGameTurn G) 543 1 rFn))))
  

;FUNCIONES REQUERIDAS

;STACKMODE
;DOMINIO: GAME
;RECORRIDO: GAME
;RECURSION: NATURAL (EN FUNCION "NCFDTAREC")
;TOMA DOS CARTAS DESDE EL DECK DEL JUEGO, Y LAS COLOCA EN EL AREA DE JUEGO DEL MISMO. LUEGO, ELIMINA LAS INSTANCIAS DESDE EL DECK.

(define stackMode
  (lambda (G)
    (if (game? G)
        (nCardsFromDeckToArea G 2)
        #f)))

;REGISTER
;DOMINIO: STRING X GAME
;RECORRIDO: GAME
;RECURSION: COLA (EN FUNCION "AHPREC")
;REGISTRA UN JUGADOR CON UNA MANO DE CARTAS VACÍA, DADO NOMBRE INGRESADO. VERIFICA QUE EL NOMBRE ESTE LIBRE Y QUE EXISTAN ESPACIOS DISPONIBLES EN EL GAME.

(define register
  (lambda (name G)
    (if (game? G)
        (addPlayerToGame G name)
        G)))

;WHOSETURNISIT?
;DOMINIO: GAME
;RECORRIDO: STRING
;SOLICITA EL ELEMENTO DEL GAME ASOCIADO AL TURNO ACTUAL.

(define whoseTurnIsIt?
  (lambda (G)
    (getGameTurn G)))

;PLAY
;DOMINIO: GAME X PROCEDURE X PROCEDURE
;RECORRIDO: GAME
;REALIZA UNA JUGADA, DADO PROCEDIMIENTO ENTREGADO ("NULL", "PASS", "SPOTIT", O "FINISH")

(define play
  (lambda (G action randFunc)
    (action G randFunc)))

;STATUS
;DOMINIO: GAME
;RECORRIDO: STRING
;SOLICITA EL ELEMENTO DEL GAME ASOCIADO AL ESTADO DEL JUEGO.

(define status
  (lambda (G)
    (getGameStatus G)))

;SCORE
;DOMINIO: GAME X STRING
;RECORRIDO: ENTERO
;RECURSION: DE COLA (EN FUNCION "GPBNFLREC")
;DETERMINA EL LARGO DE LA LISTA "CARDS" DE UN JUGADOR, CALCULANDO ASI EL PUNTAJE ASOCIADO A DICHO JUGADOR.

(define score
  (lambda (G name)
    (getPlayerScore (getPlayerByNameFromList (getGamePlayers G) name))))

;ADDCARD
;DOMINIO: LISTA DE CARTAS X CARTA
;RECORRIDO: LISTA DE CARTAS
;CONCATENA UNA CARTA ENTREGADA CON UNA LISTA DE CARTAS ENTREGADA.

(define addCard
  (lambda (CS C)
    (if (dobble? (cons C CS))
        (cons C CS)
        CS)))


(stackMode '(("AREA DE JUEGO" ())
  ((1 (1 2 3) (1 2 3) (("element" "D") ("element" "C") ("element" "B") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "G") ("element" "F") ("element" "E") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "J") ("element" "I") ("element" "H") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "H") ("element" "E") ("element" "B")))
   (1 (1 2 3) (1 2 3) (("element" "L") ("element" "I") ("element" "F") ("element" "B")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "J") ("element" "G") ("element" "B")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "J") ("element" "F") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "L") ("element" "H") ("element" "G") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "L") ("element" "J") ("element" "E") ("element" "D")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "H") ("element" "F") ("element" "D")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "I") ("element" "G") ("element" "D"))))
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "voidname"))

(stackMode '(("AREA DE JUEGO" ((1 (1 2 3) (1 2 3) (("element" "D") ("element" "C") ("element" "B") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "G") ("element" "F") ("element" "E") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "J") ("element" "I") ("element" "H") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "H") ("element" "E") ("element" "B")))))
  ()
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "voidname"))

(stackMode '(("AREA DE JUEGO" ((1 (1 2 3) (1 2 3) (("element" "D") ("element" "C") ("element" "B") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "G") ("element" "F") ("element" "E") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "J") ("element" "I") ("element" "H") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "H") ("element" "E") ("element" "B")))))
  ((1 (1 2 3) (1 2 3) (("element" "L") ("element" "I") ("element" "F") ("element" "B")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "J") ("element" "G") ("element" "B")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "J") ("element" "F") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "L") ("element" "H") ("element" "G") ("element" "C")))
   (1 (1 2 3) (1 2 3) (("element" "L") ("element" "J") ("element" "E") ("element" "D")))
   (1 (1 2 3) (1 2 3) (("element" "M") ("element" "H") ("element" "F") ("element" "D")))
   (1 (1 2 3) (1 2 3) (("element" "K") ("element" "I") ("element" "G") ("element" "D"))))
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "voidname"))


(register "famine" (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C") ("element" "D") ("element" "E")
                                                         ("element" "F") ("element" "G") ("element" "H") ("element" "I")
                                                         ("element" "J") ("element" "K") ("element" "L") ("element" "M")) 4)
                         stackMode myRandom))

(register "death" (register "famine" (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C") ("element" "D")
                                                                           ("element" "E") ("element" "F") ("element" "G")
                                                                           ("element" "H") ("element" "I") ("element" "J")
                                                                           ("element" "K") ("element" "L") ("element" "M")) 4)
                                           emptyHandStackMode myRandom)))

(register "war" (register "death" (register "famine" (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C")
                                                                                           ("element" "D") ("element" "E")
                                                                                           ("element" "F") ("element" "G")
                                                                                           ("element" "H") ("element" "I")
                                                                                           ("element" "J") ("element" "K")
                                                                                           ("element" "L") ("element" "M")) 4)
                                                           emptyHandStackMode myRandom))))

(whoseTurnIsIt? '(("AREA DE JUEGO" ())
  ()
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "famine"))

(whoseTurnIsIt? '(("AREA DE JUEGO" ())
  ()
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "war"))

(whoseTurnIsIt? '(("AREA DE JUEGO" ())
  ()
  (("famine" ()) ("death" ()) ("war" ()) ("voidname" ()))
  "starting"
  stackMode
  myRandom
  "plague"))


(play (register "plague"
                (register "war" (register "death"
                                          (register "famine"
                                                    (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C")
                                                                                          ("element" "D") ("element" "E")
                                                                                          ("element" "F") ("element" "G")
                                                                                          ("element" "H") ("element" "I")
                                                                                          ("element" "J") ("element" "K")
                                                                                          ("element" "L") ("element" "M")) 4)
                                                          emptyHandStackMode myRandom))))) null myRandom)

(play (play (register "plague"
                (register "war" (register "death"
                                          (register "famine"
                                                    (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C")
                                                                                          ("element" "D") ("element" "E")
                                                                                          ("element" "F") ("element" "G")
                                                                                          ("element" "H") ("element" "I")
                                                                                          ("element" "J") ("element" "K")
                                                                                          ("element" "L") ("element" "M")) 4)
                                                          emptyHandStackMode myRandom))))) null myRandom) (spotit '("element" "C")) myRandom)


(play (play (play
             (register "plague"
                       (register "war"
                                 (register "death"
                                           (register "famine"
                                                     (game 4 (dobbleDeck '(("element" "A") ("element" "B") ("element" "C")
                                                                                           ("element" "D") ("element" "E")
                                                                                           ("element" "F") ("element" "G")
                                                                                           ("element" "H") ("element" "I")
                                                                                           ("element" "J") ("element" "K")
                                                                                           ("element" "L") ("element" "M")) 4)
                                                           emptyHandStackMode myRandom))))) null myRandom) (spotit '("element" "C")) myRandom) null myRandom)


(status '(("AREA DE JUEGO"
   ())
  (("famine" ())
   ("death"
    ((1 (1 2 3) (1 2 3) (("element" "L") ("element" "I") ("element" "F") ("element" "B")))
     (1 (1 2 3) (1 2 3) (("element" "G") ("element" "F") ("element" "E") ("element" "A")))))
   ("war"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "J") ("element" "G") ("element" "B")))
     (1 (1 2 3) (1 2 3) (("element" "J") ("element" "I") ("element" "H") ("element" "A")))))
   ("plague"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
     (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
  "in process"
  emptyHandStackMode
  myRandom
  "famine"))

(status '(("AREA DE JUEGO"
   ())
  (("famine" ())
   ("death"
    ((1 (1 2 3) (1 2 3) (("element" "L") ("element" "I") ("element" "F") ("element" "B")))
     (1 (1 2 3) (1 2 3) (("element" "G") ("element" "F") ("element" "E") ("element" "A")))))
   ("war"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "J") ("element" "G") ("element" "B")))
     (1 (1 2 3) (1 2 3) (("element" "J") ("element" "I") ("element" "H") ("element" "A")))))
   ("plague"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
     (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
  "empate: death, war, plague."
  emptyHandStackMode
  myRandom
  "famine"))

(status '(("AREA DE JUEGO"
   ())
  (("famine" ())
   ("death" ())
   ("war" ())
   ("plague"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
     (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
  "ganador: plague"
  emptyHandStackMode
  myRandom
  "famine"))

(score '(("AREA DE JUEGO"
   ())
         ()
         (("famine" ())
          ("death" ())
          ("war" ())
          ("plague"
           ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
            (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
  "ganador: plague"
  emptyHandStackMode
  myRandom
  "famine") "famine")

(score '(("AREA DE JUEGO"
          ())
         ()
         (("famine" ())
          ("death" ())
          ("war" ())
          ("plague"
           ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
            (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
         "ganador: plague"
         emptyHandStackMode
         myRandom
         "famine") "plague")

(score '(("AREA DE JUEGO"
          ())
         ()
         (("famine" ())
          ("death" ())
          ("war" ())
   ("plague"
    ((1 (1 2 3) (1 2 3) (("element" "M") ("element" "I") ("element" "E") ("element" "C")))
     (1 (1 2 3) (1 2 3) (("element" "M") ("element" "L") ("element" "K") ("element" "A"))))))
         "ganador: plague"
         emptyHandStackMode
         myRandom
         "famine") "war")

(addCard '((1 (1 2 3) (1 2 3) (("element" "K") ("element" "L") ("element" "D") ("element" "M")))
           (1 (1 2 3) (1 2 3) (("element" "C") ("element" "B") ("element" "A") ("element" "M"))))
         '(1 (1 2 3) (1 2 3) (("element" "G") ("element" "I") ("element" "H") ("element" "M"))))
(addCard '() '(1 (1 2 3) (1 2 3) (("element" "C") ("element" "B") ("element" "A") ("element" "M"))))
(addCard '((1 (1 2 3) (1 2 3) (("element" "F") ("element" "A") ("element" "C")))
           (1 (1 2 3) (1 2 3) (("element" "D") ("element" "G") ("element" "C")))
           (1 (1 2 3) (1 2 3) (("element" "E") ("element" "B") ("element" "C")))
           (1 (1 2 3) (1 2 3) (("element" "B") ("element" "G") ("element" "A")))
           (1 (1 2 3) (1 2 3) (("element" "E") ("element" "G") ("element" "F"))))
  '(1 (1 2 3) (1 2 3) (("element" "B") ("element" "D") ("element" "F"))))


