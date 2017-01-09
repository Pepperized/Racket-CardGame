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
    (read-bitmap image))

    (define/public (get-mana)
    mana)
    
    (super-new)
    )
  )
 
(define cards '())

(define addCard (λ (-name -mana [-image "Images/Cards/default.jpg"])
                  (set! cards
                        (append cards (list
                                 (new card% [name -name] [mana -mana] [image -image])
                                 )))))

(addCard "Lightning Bolt" 1 "image.jpg")
(addCard "Bear" 2)
(addCard "Bob" 3)

(provide cards
         card%)

