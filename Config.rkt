#lang racket

(define P1Health 15)
(define P2Health 15)

(define currentPlayerY -100)
(define enemyPlayerY 200)
(define currentPlayerCreatureLifeY 400)
(define enemyPlayerCreatureLifeY 100)
(define noCreature 10)
(define maxMana 5)

(define fiveCardHandRange  (list (list 55 295)  (list 320 555) (list 580 820) (list 840 1080) (list 1105 1345)))
(define fourCardHandRange  (list (list 185 425) (list 450 685) (list 710 950) (list 975 1210)))
(define threeCardHandRange (list (list 320 555) (list 580 820) (list 840 1080)))
(define twoCardHandRange   (list (list 450 685) (list 710 950)))                
(define oneCardHandRange   (list (list 580 820)))                                                          

(define oneCreatureBoard   (list (list 615 785)))
(define twoCreatureBoard   (list (list 515 685) (list 715 885)))
(define threeCreatureBoard (list (list 415 585) (list 615 785) (list 815 985)))
(define fourCreatureBoard  (list (list 315 485) (list 515 685) (list 715 885) (list 915 1085)))
(define fiveCreatureBoard  (list (list 215 385) (list 415 585) (list 615 785) (list 815 985) (list 1015 1185)))

(define currentPlayerY-BoardRange (list 375 620))
(define enemyPlayerY-BoardRange (list 75 320))

(struct spellStruct (name effect num) #:mutable)
(define activeSpell (spellStruct "none" "none" "none"))

(define player1SelectedFriendlyCreature (λ (lst)
                                  (first (first lst))))
(define player1SelectedEnemyCreature (λ (lst)
                                   (second (first lst))))
(define player2SelectedFriendlyCreature (λ (lst)
                                  (second (second lst))))
(define player2SelectedEnemyCreature (λ (lst)
                                   (first (second lst))))

(define playerDamage (λ (attack turn)
          (cond
            ((equal? turn 1)
             (set! P2Health (- P2Health attack)))
            ((equal? turn 2)
             (set! P1Health (- P1Health attack))))
           (displayln P1Health)
           (displayln P2Health)))

(provide (all-defined-out))
