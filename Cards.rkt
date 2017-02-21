#lang racket

(require racket/class
         racket/draw
         "Classes.rkt")
 
(define cards '())

(define addCard (λ (card)
                  (set! cards
                        (append cards (list
                                 card)))))
                          
(addCard (new spell% [name "Lightning"] [mana 1] [image "Lightning.png"]))
(addCard (new creature% [name "Vicious Beaver"] [mana 1] [attack 2] [life 1] [image "Vicious Beaver.png"]))
(addCard (new creature% [name "Imp"] [mana 1] [attack 3] [life 2] [image "Imp.png"]))
(addCard (new spell% [name "Bounce"] [mana 1] [image "Bounce.png"]))
(addCard (new creature% [name "Archer"] [mana 2] [attack 2] [life 2] [image "Archer.png"]))
(addCard (new creature% [name "Bear"] [mana 2] [attack 3] [life 2] [image "Bear.png"]))
(addCard (new spell% [name "Cull"] [mana 2] [image "Cull.png"]))
(addCard (new creature% [name "Magma Rager"] [mana 2] [attack 5] [life 1] [image "Magma Rager.png"]))
(addCard (new creature% [name "Priestess"] [mana 2] [attack 2] [life 2] [image "Priestess.png"]))
(addCard (new spell% [name "Assassinate"] [image "Assassinate.png"]))
(addCard (new creature% [name "Cultist"] [mana 3] [attack 4] [life 2] [image "Cultist.png"]))
(addCard (new creature% [name "Paladin"] [mana 3] [attack 3] [life 4] [image "Paladin.png"]))
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
