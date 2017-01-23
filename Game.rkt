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

(define hand '())
              
(define drawCard (λ () (set! hand (append hand (list (list-ref deck (random (length deck))))))))

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

(define removeCardFromHand (λ (index)
                             (set! hand (remove (list-ref hand index) hand))
                             ))


(define init (λ ()
               (drawCard)
               (drawCard)
               (drawCard)
               (drawCard)
               )
  )

(init)

(define packageCardObject (λ (card)
                        (new creatureObject% [card (first hand)] [index 0])
                        ))

(send (first hand) get-image)

(provide deck
         hand
         init
         packageCardObject
         removeCardFromHand)

;(init)