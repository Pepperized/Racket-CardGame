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
    (init-field [sleep #t])
    (init-field [on-play-effect "none"])

    (define/public (get-attack) attack)
    (define/public (get-life) life)
    (define/public (get-on-play-effect) on-play-effect)
    (define/public (get-sleep) sleep)
    (define/public (set-sleep x) (set! sleep x))
    
    (super-new)
    )
  )

(define creatureObject%
  (class object%
    (init-field [card (new creature%)])
    (init-field [index 0])
    (init-field [player 1])
    (init-field [currentHealth 0])

    (define/public (get-card)
      card)
    (define/public (get-index)
      index)
    (define/public (set-index x)
      (set! index x))
    (define/public (get-player)
      player)
    (define/public (set-health x)
      (set! currentHealth 0))
    (define/public (change-health x)
      (set! currentHealth (+ currentHealth x))
      )
    (define/public (get-currentHealth)
      currentHealth)

    (set-health (send card get-life))
    
    (super-new)
    )
  )

(provide (all-defined-out))
