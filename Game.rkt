#lang racket/gui

(require racket/draw
         net/url)

(require "Cards.rkt"
         (prefix-in config: "Config.rkt"))

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

(struct mana (currentMana manaCap) #:mutable)
(define P1Mana (mana 1 1))
(define P2Mana (mana 0 0))

(define currentTurn 1)
(struct spellStruct (name effect) #:mutable)
(define activeSpell (spellStruct "none" "none"))

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
(define cancelSpell (λ ()
                      (cond
                        ((equal? currentTurn 1)
                         (set! P1hand (append P1hand (list (getCard (spellStruct-name activeSpell)))))
                         (set-spellStruct-name! activeSpell "none")
                         (set-spellStruct-effect! activeSpell "none"))
                        ((equal? currentTurn 2)
                         (set! P2hand (append P2hand (list (getCard (spellStruct-name activeSpell)))))
                         (set-spellStruct-name! activeSpell "none")
                         (set-spellStruct-effect! activeSpell "none")))))
(define endTurn (λ (manaDisplay)
                  (cond
                    ((equal? currentTurn 1)
                     (begin (set! currentTurn 2)
                            (cond
                              ((> config:maxMana (mana-manaCap P2Mana)) (begin (set-mana-manaCap! P2Mana (+ 1 (mana-manaCap P2Mana)))) (set-mana-currentMana! P2Mana (mana-manaCap P2Mana)) (P2drawCard))
                              ((equal? config:maxMana (mana-manaCap P2Mana)) (set-mana-currentMana! P2Mana (mana-manaCap P2Mana)) 
                                                                             (cond
                                                                               ((not (>= (length P1hand) 5)) (P2drawCard)))))
                            (send manaDisplay set-label (string-append "mana:" (number->string (mana-currentMana P2Mana)) "/" (number->string (mana-manaCap P2Mana))))))
                    ((equal? currentTurn 2)
                     (begin (set! currentTurn 1)
                            (cond
                              ((> config:maxMana (mana-manaCap P1Mana)) (begin (set-mana-manaCap! P1Mana (+ 1 (mana-manaCap P1Mana))) (set-mana-currentMana! P1Mana (mana-manaCap P1Mana)) (P1drawCard)))
                              ((equal? config:maxMana (mana-manaCap P1Mana)) (set-mana-currentMana! P1Mana (mana-manaCap P1Mana)) 
                                                                             (cond
                                                                               ((not (>= (length P1hand) 5)) (P1drawCard)))))
                            (send manaDisplay set-label (string-append "mana:" (number->string (mana-currentMana P2Mana)) "/" (number->string (mana-manaCap P2Mana)))))))))

(provide deck
         P1hand P2hand
         P1Mana P2Mana
         mana-currentMana set-mana-currentMana!
         mana-manaCap set-mana-manaCap!
         currentTurn
         cancelSpell activeSpell set-spellStruct-name! spellStruct-name set-spellStruct-effect! spellStruct-effect
         endTurn
         init
         packageCardObject
         removeCardFromHand)