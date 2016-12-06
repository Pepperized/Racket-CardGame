#lang racket

(require racket/class)

(define card%
  (class object%
    (init-field [name "No Name"])
    (init-field [mana 0])

    (define/public (get-name)
    name)

    (define/public (get-mana)
    mana)
    
    (super-new)
    )
  )
 
(define cards '())

(define addCard (λ (-name -mana)
                  (set! cards
                        (append cards (list
                                 (new card% [name -name] [mana -mana])
                                 )))))

(addCard "Lightning Bolt" 1)
(addCard "Bear" 2)
(addCard "Bob" 3)


