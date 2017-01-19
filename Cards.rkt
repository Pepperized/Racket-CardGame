#lang racket

(require racket/class
         racket/draw)

(define card%
  (class object%
    (init-field [name "No Name"])
    (init-field [mana 0])
    (init-field [image "Images/Cards/default.jpg"])

    (define/public (get-name)
    name)

    (define/public (get-image)
    (read-bitmap (string-append "Images/Cards/" image)))

    (define/public (get-mana)
    mana)
    
    (super-new)
    )
  )

(define spell%
  (class card%
    (init-field [effect "none"])

    (define/public (get-effect)
      effect)
    
    (super-new)
    )
  )

(define creature%
  (class card%
    (init-field [attack 0])
    (init-field [life 0])

    (init-field [on-play-effect "none"])

    (define/public (get-attack) attack)
    (define/public (get-life) life)
    (define/public (get-on-play-effect) on-play-effect)
    
    (super-new)
    )
  )
 
(define cards '())

(define addCard (λ (card)
                  (set! cards
                        (append cards (list
                                 card
                                 )))))

(addCard (new spell% [name "Lightning"] [mana 1] [image "Lightning.png"]))
(addCard (new creature% [name "Vicious Beaver"] [mana 1] [image "Vicious Beaver.png"]))
(addCard (new creature% [name "Imp"] [mana 1] [image "Imp.png"]))
(addCard (new spell% [name "Bounce"] [mana 1] [image "Bounce.png"]))
(addCard (new creature% [name "Archer"] [mana 2] [image "Archer.png"]))
(addCard (new creature% [name "Bear"] [mana 2] [image "Bear.png"]))
(addCard (new spell% [name "Cull"] [mana 2] [image "Cull.png"]))
(addCard (new creature% [name "Magma Rager"] [mana 2] [image "Magma Rager.png"]))
(addCard (new creature% [name "Priestess"] [mana 2] [image "Priestess.png"]))
(addCard (new creature% [name "Assassin"] [mana 3] [image "Assassin.png"]))
(addCard (new creature% [name "Cultist"] [mana 3] [image "Cultist.png"]))
(addCard (new creature% [name "Paladin"] [mana 3] [image "Paladin.png"]))
(addCard (new spell% [name "Lava Flow"] [mana 4] [image "Lava Flow.png"]))

(define getCard (λ (name [card-list cards]) (cond
                            ((= 0 (length card-list)) (error "Failed to find card."))
                            ((equal? name (send (first card-list) get-name)) (first card-list))
                            (#t (getCard name (rest card-list)))
                            )
                  )
  )

(provide getCard
         card%)

;(send (list-ref cards 4) get-image)

;(define bolt (new spell% [name "Lightning"] [mana 1] [image "Lightning.png"]))
