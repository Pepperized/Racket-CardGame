#lang racket

(require racket/class
         racket/draw
         "Classes.rkt"
         (prefix-in config: "Config.rkt"))


(define cards '())

(define addCard (λ (card)
                  (set! cards
                        (append cards (list
                                 card)))))

(define dealDamage (λ (target [damage (config:spellStruct-num config:activeSpell)])
                   (send target set-life (- (send target get-life) damage))))

(define impEffect (λ (turn)
                    (cond
                      ((equal? turn 1)
                       (config:playerDamage 3 2))
                      ((equal? turn 2)
                       (config:playerDamage 3 1)))))

(define archerEffect (λ (turn)
                       (config:set-spellStruct-num! config:activeSpell 1)
                       (config:set-spellStruct-effect! config:activeSpell dealDamage)))
(define lightningEffect (λ ()
                          (config:set-spellStruct-num! config:activeSpell 2)
                          (config:set-spellStruct-effect! config:activeSpell dealDamage)
                          (config:set-spellStruct-name! config:activeSpell "Lightning")))
(define paladinEffect (λ (player)
                        (cond
                          ((equal? player 1)
                           (config:playerDamage -3 1))
                          ((equal? player 2)
                           (config:playerDamage -3 2)))))

(addCard (new spell% [name "Lightning"] [mana 1] [image "Lightning.png"] [effect lightningEffect]))
(addCard (new creature% [name "Vicious Beaver"] [mana 1] [attack 2] [life 1] [image "Vicious Beaver.png"]))
(addCard (new creature% [name "Imp"] [mana 1] [attack 3] [life 2] [image "Imp.png"] [on-play-effect impEffect]))
(addCard (new spell% [name "Bounce"] [mana 1] [image "Bounce.png"]))
(addCard (new creature% [name "Archer"] [mana 2] [attack 2] [life 2] [image "Archer.png"] [on-play-effect archerEffect]))
(addCard (new creature% [name "Bear"] [mana 2] [attack 3] [life 2] [image "Bear.png"]))
(addCard (new spell% [name "Cull"] [mana 2] [image "Cull.png"]))
(addCard (new creature% [name "Magma Rager"] [mana 2] [attack 5] [life 1] [image "Magma Rager.png"]))
(addCard (new creature% [name "Priestess"] [mana 2] [attack 2] [life 2] [image "Priestess.png"]))
(addCard (new spell% [name "Assassinate"] [image "Assassinate.png"]))
(addCard (new creature% [name "Cultist"] [mana 3] [attack 4] [life 2] [image "Cultist.png"]))
(addCard (new creature% [name "Paladin"] [mana 3] [attack 3] [life 4] [image "Paladin.png"] [death-effect paladinEffect]))
(addCard (new spell% [name "Lava Flow"] [mana 4] [image "Lava Flow.png"]))

(define getCard (λ (name [card-list cards]) (cond
                            ((= 0 (length card-list)) (error "Failed to find card."))
                            ((equal? name (send (first card-list) get-name)) (first card-list))
                            (#t (getCard name (rest card-list)))
                            )))

(provide getCard
         creature%
         spell%
         card%)

;(send (list-ref cards 4) get-image)

;(define bolt (new spell% [name "Lightning"] [mana 1] [image "Lightning.png"]))
