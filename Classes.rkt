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
    (inherit-field name mana image)

    (define/public (get-effect)
      effect)
    (define/public (clone)
      (new spell% [effect effect] [name name] [mana mana] [image image]))   
    (super-new)
    )
  )

(define creature%
  (class card%
    (init-field [attack 0])
    (init-field [life 0])
    (init-field [sleep #t])
    (init-field [on-play-effect "none"])
    (init-field [death-effect "none"])
    (inherit-field name mana image)

    (define/public (get-attack) attack)
    (define/public (get-life) life)
    (define/public (set-life x) (set! life x))
    (define/public (get-on-play-effect) on-play-effect)
    (define/public (get-death-effect) death-effect)
    (define/public (get-sleep) sleep)
    (define/public (set-sleep x) (set! sleep x))
    (define/public (clone)
      (new creature% [attack attack] [life life] [on-play-effect on-play-effect] [death-effect death-effect] [sleep sleep] [name name] [mana mana] [image image]))
    
    (super-new)
    )
  )

(define creatureObject%
  (class object%
    (init-field [card (new creature%)])
    (init-field [index 0])
    (init-field [player 1])
    (init-field [lifeDisplay "none"])
    (define/public (get-card)
      card)
    (define/public (get-index)
      index)
    (define/public (set-index x)
      (set! index x))
    (define/public (get-player)
      player)
    (define/public (get-lifeDisplay)
      lifeDisplay)
    (define/public (set-lifeDisplay x)
      (set! lifeDisplay x))
    (super-new)
    )
  )

(provide (all-defined-out))
