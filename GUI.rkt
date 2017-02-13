#lang racket/gui

(require racket/draw
         net/url
         mred
         (only-in mrlib/image-core render-image)
         "Game.rkt"
         "Cards.rkt"
         "Helpers.rkt"
         (prefix-in config: "Config.rkt"))

(require (prefix-in htdp: 2htdp/image))

(struct gameObject (image x y scale layer player) #:mutable)

(define logo
  (read-bitmap  "Image.jpg"))


(define frame (new frame%
                   [label "Window"] [width 1400] [height 800]
                   [style '(no-resize-border)]
                   ))




;;(define image->bitmap (λ (x) (htdp:color-list->bitmap (htdp:image->color-list x) (htdp:image-width x) (htdp:image-height x))))

;; Let's make a larger bitmap.
(define scale-bitmap (λ (image sc) (let ([result
                                          (make-bitmap (inexact->exact (round (* sc (send image get-width))))
                                                       (inexact->exact (round (* sc (send image get-height)))
                                                                       ))])
                       (let ([dc (new bitmap-dc% [bitmap result])])
                       (send dc scale sc sc)
                       (send dc set-alpha 1)
                       (send dc draw-bitmap logo 0 0)
                         result
                       ))))




(define turn 1)

(define objects '())
(define sortedObjects '())
(define player1ObjectIndex '())
(define creaturesPlayer1 '())
(define creaturesPlayer2 '())
(define creatureObjects '())
;(define Creature1 (new creatureObject%))
;(define Creature2 (new creatureObject%))

(define sortPlayerCreatures (λ ([x creatureObjects] [y creaturesPlayer1] [z creaturesPlayer2])
                              (cond
                                ((equal? 0 (length x)) (values y z))
                                ((equal? 1 (send (first x) get-player)) (begin
                                                                     (set! y (append (list (first x)) y)) (sortPlayerCreatures (rest x) y z)))
                                ((equal? 2 (send (first x) get-player)) (begin
                                                                     (append (first x) z) (sortPlayerCreatures (rest x) y z))))))


(define addObject (λ (image x y scale layer [player 0])
                    (set! objects (append objects (list (gameObject image x y scale layer player))))
                    (send canvas on-paint)
                    ))

(define removeObject (λ (index)
                       (set! objects (remove (list-ref objects index) objects))
                       (send canvas on-paint)
                       ))

(define sortObjects (λ ([count 0] [res '()]) (cond
                                     ((= count 10) res)
                                     (#t (let ([x '()])
                                           (for ([i (length objects)])
                                             (when (= count (gameObject-layer (list-ref objects i))) (set! x (append x (list (list-ref objects i)))))
                                                                                                           
                                           )
                                           (sortObjects (+ count 1) (append res x))
                                           )
                                         )
                                     )
                      ))

(define generateGameScene (λ () 
                            (set! sortedObjects (sortObjects))
                            (let ([bg (htdp:bitmap/file "Images/Other/bg.jpg")])
                              (overlay (htdp:overlay/offset (htdp:bitmap/file "Images/Other/divider.png") 0 50 bg))
                              )
                            ))
                            
(define overlay (λ (i1 [i 0]) (cond
                                       ((= i (length sortedObjects)) i1)
                                       (#t (overlay (htdp:overlay/offset (htdp:scale (gameObject-scale (list-ref sortedObjects i)) (gameObject-image (list-ref sortedObjects i))) (gameObject-x (list-ref sortedObjects i)) (gameObject-y (list-ref sortedObjects i)) i1) (+ i 1)))
                                       )))
                  



(define (image->bitmap image)
   (let* ([width (htdp:image-width image)]
         [height (htdp:image-height image)]
         [bm (make-bitmap width height)]
          [dc (make-object bitmap-dc% bm)])
     (send dc clear)
     (render-image image dc 0 0)
     bm))
           
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (init-field [bitmapX 0])
    (init-field [bitmapY 0])
    (init-field [bitmap-scale 1])
    (inherit get-dc)
    (define/override (on-paint)

      (send (get-dc) draw-bitmap
            
            (image->bitmap (htdp:scale bitmap-scale (generateGameScene)))
            bitmapX bitmapY
            )
      )
    
    (define/override (on-event event)
      (cond
        ((send event button-down? 'left) (println (string-append "X: " (number->string (send event get-x)) ", Y: " (number->string (send event get-y)))))
      )
      )
     
    (super-new)))



(define canvas (new bitmap-canvas% [parent frame] [bitmap logo] [bitmap-scale 1] [min-height 600]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define bottomFrame (new horizontal-panel% [parent frame] [alignment '(right bottom)]))

; Make a button in the frame
(new button% [parent bottomFrame]
             [label "Play Card"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (playFirstCard))]
             )

(new button% [parent bottomFrame]
             [label "Show Hand"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send handFrame show #t))])

(new button% [parent bottomFrame]
             [label "End Turn"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (cond
                           ((= turn 1) (set! turn 2))
                           ((= turn 2) (set! turn 1))
                           )
                         (refreshBoard)
                         )])
(define menu-bar (new menu-bar%
                      (parent frame)))

(define menu-game (new menu%
     (label "&Game")
     (parent menu-bar)))

(define menu-about (new menu%
     (label "&About")
     (parent menu-bar)))

(define choice1 (new menu-item%
                     [parent menu-game] [label "New Game"] [callback (lambda (button event)
                         (send msg set-label "Button click"))]))
  
(define documentation (new menu-item%
                     [parent menu-about] [label "Documentation"] [callback (lambda (button event)
                         (send msg set-label "Docs"))]))


(define startGUI (λ ()
                   (send frame show #t)
                   ))



(provide startGUI
         addObject)

(define greenUnderlay (htdp:rectangle 250 380 "solid" "green"))

(define redUnderlay (htdp:rectangle 250 380 "solid" "red"))

(define hitEffect (htdp:rotate 10 (htdp:scale/xy 3 4 (htdp:overlay (htdp:star-polygon 15 10 3 "solid" "red")
                                (htdp:star-polygon 20 10 3 "solid" "yellow")  
                                (htdp:star-polygon 20 10 3 "outline" "black")
                                )))
  )

;(addObject (htdp:bitmap/file "image.jpg") 0 200 0.7 1)
;(addObject greenUnderlay 200 0 1 2)
;(addObject redUnderlay 200 0 1 0)
;(addObject (htdp:bitmap/file "image.jpg") 0 -100 0.7 1)

(define handFrame (new frame%
                   [label "Window"] [width 1400] [height 400]
                   [style '(no-resize-border)]
                   ))

           
(define hand-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (init-field [bitmapX 0])
    (init-field [bitmapY 0])
    (init-field [bitmap-scale 1])
    (inherit get-dc)
    (define/override (on-paint)
      (cond
        
      ((not (= 0 (length hand))) (send (get-dc) draw-bitmap
            
            (image->bitmap (htdp:scale bitmap-scale (htdp:overlay (htdp:scale 0.7 (generateHandImage (send (first hand) get-image))) (htdp:bitmap/file "Images/Other/bg_hand.jpg"))))
            bitmapX bitmapY  
            ))
      (#t (send (get-dc) draw-bitmap (image->bitmap (htdp:bitmap/file "Images/Other/bg_hand.jpg")) bitmapX bitmapY))
      )
      )

    (define/public (playClickedCard event)
      (let ([x (send event get-x)])
        (let ([y (send event get-y)])
          (cond
            ((and (< y 25) (> y 370)) #f)
            ((equal? (length hand) 0) #f)
            ((equal? (length hand) 1)
             (cond ((and (>= x (first (first config:oneCardHandRange))) (<= x (second (first config:oneCardHandRange)))) (playCard 0)))) 
             ((equal? (length hand) 2)
              (for ([i (length config:twoCardHandRange)])
                (cond ((and (>= x (first (list-ref config:twoCardHandRange i))) (<= x (second (list-ref config:twoCardHandRange i)))) (playCard i)))))
             ((equal? (length hand) 3)
              (for ([i (length config:threeCardHandRange)])
                (cond ((and (>= x (first (list-ref config:threeCardHandRange i))) (<= x (second (list-ref config:threeCardHandRange i))) (playCard i))))))
             ((equal? (length hand) 4)
              (for ([i (length config:fourCardHandRange)])
                (cond ((and (>= x (first (list-ref config:fourCardHandRange i))) (<= x (second (list-ref config:fourCardHandRange i))) (playCard i)))))) 
             ((equal? (length hand) 5)
              (for ([i (length config:fiveCardHandRange)])
                (cond ((and (>= x (first (list-ref config:fiveCardHandRange i))) (<= x (second (list-ref config:fiveCardHandRange i))) (playCard i)))))) 
            ))))
            
    (define/override (on-event event)
      (cond
        ((send event button-down? 'left) (playClickedCard event))
      )
      )
    (super-new)))
(define hand-canvas (new hand-canvas% [parent handFrame] [bitmap logo] [bitmap-scale 1] [min-height 400]))

(define generateHandImage (λ (image [i 1]) (cond
                                  ((= (length hand) i) image)
                                  (#t (generateHandImage (htdp:beside image (send (list-ref hand i) get-image)) (+ i 1)))
                                )
                            )
  )

(define sortPlayer1CreatureIndex (λ ()
                                   (for ([i (length sortedObjects)])
                               (when (equal? (gameObject-player (list-ref sortedObjects i)) 1)
                                 (begin
                                   (set! player1ObjectIndex (append player1ObjectIndex (list i)))
                                   (set-gameObject-y! (list-ref sortedObjects i) (getPlayerY 1))
                                   )))
                                   (set! player1ObjectIndex (removed2 player1ObjectIndex))))
(define refreshBoard (λ ()
                       (sortPlayer1CreatureIndex)
                          (cond
                            ((equal? (length creaturesPlayer1) 1) #t)
                            ((equal? (length creaturesPlayer1) 2)
                             (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 100)
                             (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) -100)
                             (send canvas on-paint)
                             (send canvas show #t))
                            ((equal? (length creaturesPlayer1) 3)
                             (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 200)
                             (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 0)
                             (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) -200)
                             (send canvas on-paint)
                             (send canvas show #t))
                            ((equal? (length creaturesPlayer1) 4)
                             (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 300)
                             (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 100)
                             (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) -100)
                             (set-gameObject-x! (list-ref sortedObjects (fourth player1ObjectIndex)) -300)
                             (send canvas on-paint)
                             (send canvas show #t))
                            ((equal? (length creaturesPlayer1) 5)
                             (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 400)
                             (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 200)
                             (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) 0)
                             (set-gameObject-x! (list-ref sortedObjects (fourth player1ObjectIndex)) -200)
                             (set-gameObject-x! (list-ref sortedObjects (fifth player1ObjectIndex)) -400)
                             (send canvas on-paint)
                             (send canvas show #t))
                            )))

(define playCard (λ (pos)
                   (cond
                     ((is-a? (list-ref hand pos) spell%)
                      ;(send msg set-label (string-append "Choose target: " (number->string (send (list-ref hand pos) get-effect))))
                      )
                     ((is-a? (list-ref hand pos) creature%)
                      (let ([x (packageCardObject (list-ref hand pos) pos)])
                        (set! creaturesPlayer1 (append creaturesPlayer1 (list x)))
                        (send x set-index (index-of creaturesPlayer1 x))
                        (addObject (send (send x get-card) get-image) 0 (getPlayerY 1) 0.5 1 1)
                        (removeCardFromHand pos)
                        (send hand-canvas on-paint)
                        (send hand-canvas show #t)
                        (refreshBoard)
                        ))
                     )))

(define playFirstCard (λ ()
                        (cond
                          ((= (length hand) 0) #f)
                          (#t (let ([x (packageCardObject (first hand))])
                          (addObject (send (send x get-card) get-image) 0 (getPlayerY 1) 0.5 1 1)
                          (set! creaturesPlayer1 (append creaturesPlayer1 (list x)))
                          (send x set-index (index-of creaturesPlayer1 x))
                          (removeCardFromHand 0)
                          (send hand-canvas on-paint)
                          (send hand-canvas show #t)
                          ))
                        )
                        ))

(define getPlayerY (λ (player) (cond
                                 ((= turn player) config:currentPlayerY)
                                 (#t config:otherPlayerY)
                                 )))