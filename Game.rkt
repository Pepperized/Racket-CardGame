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
    (init-field [image (new creature%)])
    (init-field [index 0])
    (init-field [player 1])
    
    (super-new)
    )
  )


(define init (λ ()
               (drawCard)
               (drawCard)
               (drawCard)
               (drawCard)
               )
  )

(init)

(send (first hand) get-image)

(provide deck
         hand
         init)

;(init)