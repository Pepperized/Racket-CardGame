#lang racket/gui

(require racket/draw
         net/url)

(require "Cards.rkt")

(define deck (list
              (getCard "Lightning")
              (getCard "Lightning")
              (getCard "Lightning")
              (getCard "Lightning")
              (getCard "Archer")
              (getCard "Archer")
              (getCard "Archer")
              (getCard "Archer")
              (getCard "Cultist")
              (getCard "Cultist")
              (getCard "Imp")
              (getCard "Imp")
              (getCard "Imp")
              (getCard "Imp")
              )
  )

(define P1hand '())
(define P2hand '())

(define P1Mana 1)
(define P1ManaCap 1)
(define P2Mana 0)
(define P2ManaCap 0)

(define currentTurn 1)

(define P1drawCard (λ () (set! P1hand (append P1hand (list (list-ref deck (random (length deck))))))))
(define P2drawCard (λ () (set! P2hand (append P2hand (list (list-ref deck (random (length deck))))))))

(define removeCardFromHand (λ (index)
                             (cond
                               ((equal? currentTurn 1)
                                (set! P1hand (remove (list-ref P1hand index) P1hand)))
                               ((equal? currentTurn 2)
                                 (set! P2hand (remove (list-ref P2hand index) P2hand)))
                               (#t (displayln "currentTurn error"))
                               )))

(define creatureObject%
  (class object%
    (init-field [card (new creature%)])
    (init-field [index 0])
    (init-field [player 1])

    (define/public (get-card)
      card)
    (define/public (get-index)
      index)
    (define/public (set-index x)
      (set! index x))
    (define/public (get-player)
      player)
    
    (super-new)
    )
  )

(define init (λ ()
               (P1drawCard)
               (P1drawCard)
               (P1drawCard)
               (P1drawCard)
               (P2drawCard)
               (P2drawCard)
               (P2drawCard)
               (P2drawCard)
               )
  )

(init)

(define packageCardObject (λ (card pos)
                            (cond
                              ((equal? currentTurn 1)
                               (new creatureObject% [card (list-ref P1hand pos)] [index 0] [player 1]))
                              ((equal? currentTurn 2)
                               (new creatureObject% [card (list-ref P2hand pos)] [index 0] [player 2]))
                              (#t (displayln "currentTurn error"))
                               )))

(define endTurn (λ ()
                  (cond
                    ((equal? currentTurn 1)
                     (begin (set! currentTurn 2) (cond
                                                   ((> 10 P2ManaCap) (begin (+ 1 P2ManaCap) (set! P2Mana P2ManaCap)(P2drawCard)))
                                                   ((equal? 10 P2ManaCap) (set! P2Mana P2ManaCap)(P2drawCard)))))
                    ((equal? currentTurn 2)
                     (begin (set! currentTurn 1) (cond
                                                  ((> 10 P1ManaCap) (begin (+ 1 P1ManaCap) (set! P1Mana P1ManaCap) (P1drawCard)))
                                                   ((equal? 10 P1ManaCap) (set! P1Mana P1ManaCap) (P1drawCard))))))))

(provide deck
         P1hand P2hand
         P1Mana P2Mana
         P1ManaCap P2ManaCap
         currentTurn
         endTurn
         init
         packageCardObject
         removeCardFromHand)