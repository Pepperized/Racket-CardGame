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

(struct gameObject (image x y scale layer) #:mutable)

(define logo
  (read-bitmap  "Image.jpg"))

(define frame (new frame%
                   [label "Window"] [width 1400] [height 800]
                   [style '(no-resize-border)]))
;;(define image->bitmap (λ (x) (htdp:color-list->bitmap (htdp:image->color-list x) (htdp:image-width x) (htdp:image-height x))))

;; Let's make a larger bitmap.
(define scale-bitmap (λ (image sc)
                       (let ([result
                              (make-bitmap
                               (inexact->exact (round (* sc (send image get-width))))
                               (inexact->exact (round (* sc (send image get-height)))))])
                         (let ([dc (new bitmap-dc% [bitmap result])])
                       (send dc scale sc sc)
                       (send dc set-alpha 1)
                       (send dc draw-bitmap logo 0 0)
                         result))))

(define healthDisplays '())
(define objects '())
(define sortedObjects '())
(define player1ObjectIndex '())
(define player2ObjectIndex '())
(define creaturesPlayer1 '())
(define creaturesPlayer2 '())
(define creatureObjects '())

(define indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
(define selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))

(define sortPlayerCreatures (λ ([x creatureObjects] [y creaturesPlayer1] [z creaturesPlayer2])
                              (cond
                                ((equal? 0 (length x)) (values y z))
                                ((equal? 1 (send (first x) get-player)) (begin
                                                                     (set! y (append (list (first x)) y)) (sortPlayerCreatures (rest x) y z)))
                                ((equal? 2 (send (first x) get-player)) (begin
                                                                     (append (first x) z) (sortPlayerCreatures (rest x) y z))))))


(define addObject (λ (image x y scale layer)
                    (set! objects (append objects (list (gameObject image x y scale layer))))
                    (send canvas on-paint)))

(define removeObject (λ (index)
                       (set! sortedObjects (remove (list-ref sortedObjects index) sortedObjects))
                       (set! objects sortedObjects)
                       (send canvas on-paint)))

(define removeCreatureObject (λ (index)
                       (set! creatureObjects (remove (list-ref creatureObjects index) creatureObjects)) 
                       (send canvas on-paint)))

(define removeCreatureLifeDisplayResult '())
(define removeCreatureLifeDisplay (λ (creatureObj)
                                    (cond
                                      ((not (equal? (send creatureObj get-lifeDisplay) "none"))
                                       (for ([i (length sortedObjects)])
                                         (when (and
                                                (equal? (gameObject-image (list-ref sortedObjects i)) (gameObject-image (send creatureObj get-lifeDisplay)))
                                                (equal? (gameObject-x (list-ref sortedObjects i)) (gameObject-x (send creatureObj get-lifeDisplay)))
                                                (equal? (gameObject-y (list-ref sortedObjects i)) (gameObject-y (send creatureObj get-lifeDisplay)))
                                                (equal? (gameObject-scale (list-ref sortedObjects i)) (gameObject-scale (send creatureObj get-lifeDisplay)))
                                                (equal? (gameObject-layer (list-ref sortedObjects i)) (gameObject-layer (send creatureObj get-lifeDisplay))))
                                           (set! removeCreatureLifeDisplayResult (append removeCreatureLifeDisplayResult (list (- i (length removeCreatureLifeDisplayResult)))))))
                                       (for ([i (length removeCreatureLifeDisplayResult)])
                                         (removeObject (list-ref removeCreatureLifeDisplayResult i)))
                                       (set! removeCreatureLifeDisplayResult '()))
                                      (#t #f))))
;(define removeObject (λ (index)
;                       (set! objects (remove (list-ref objects index) objects))
;                       (send canvas on-paint)))

(define sortObjects (λ ([count 0] [res '()]) (cond
                                     ((= count 10) res)
                                     (#t (let ([x '()])
                                           (for ([i (length objects)])
                                             (when (= count (gameObject-layer (list-ref objects i))) (set! x (append x (list (list-ref objects i))))))
                                           (sortObjects (+ count 1) (append res x)))))))                

(define generateGameScene (λ () 
                            (set! sortedObjects (sortObjects))
                            (let ([bg (htdp:bitmap/file "Images/Other/bg.jpg")])
                              (overlay (htdp:overlay/offset (htdp:bitmap/file "Images/Other/divider.png") 0 50 bg))
                              )))
                            
(define overlay (λ (i1 [i 0]) (cond
                                ((= i (length sortedObjects)) i1)
                                (#t (overlay (htdp:overlay/offset (htdp:scale (gameObject-scale (list-ref sortedObjects i)) (gameObject-image (list-ref sortedObjects i))) (gameObject-x (list-ref sortedObjects i)) (gameObject-y (list-ref sortedObjects i)) i1) (+ i 1))))))

(define (image->bitmap image)
   (let* ([width (htdp:image-width image)]
         [height (htdp:image-height image)]
         [bm (make-bitmap width height)]
          [dc (make-object bitmap-dc% bm)])
     (send dc clear)
     (render-image image dc 0 0)
     bm))

(struct temp (lst) #:mutable)
(define tmp (temp '()))
(struct coords (x y) #:mutable)
(define currentCoords (coords 0 0))
(define convertCoordsPrelim (λ (y turn)
                              (cond
                                ((and (equal? turn 1) (and (>= y (first config:currentPlayerY-BoardRange)) (<= y (second config:currentPlayerY-BoardRange))))
                                 (set-temp-lst! tmp creaturesPlayer1)
                                 (set-coords-y! currentCoords (- y 600)))
                                ((and (equal? turn 1) (and (>= y (first config:enemyPlayerY-BoardRange)) (<= y (second config:enemyPlayerY-BoardRange))))
                                 (set-temp-lst! tmp creaturesPlayer2))
                                ((and (equal? turn 2) (and (>= y (first config:currentPlayerY-BoardRange)) (<= y (second config:currentPlayerY-BoardRange))))
                                 (set-temp-lst! tmp creaturesPlayer2)
                                 (set-coords-y! currentCoords (- y 600)))
                                ((and (equal? turn 2) (and (>= y (first config:enemyPlayerY-BoardRange)) (<= y (second config:enemyPlayerY-BoardRange))))
                                 (set-temp-lst! tmp creaturesPlayer1)))))

(define convertCoords (λ (x creaturesLst1 creaturesLst2 turn temp)
                        (cond
                          ((equal? (length (temp-lst tmp)) 1) (set-coords-x! currentCoords (- x 700)))
                          ((equal? (length (temp-lst tmp)) 2)
                           (if (and (>= x 515) (<= x 685)) (set-coords-x! currentCoords (- x 500)) (set-coords-x! currentCoords (- x 900))))
                          ((equal? (length (temp-lst tmp)) 3)
                           (if (and (>= x 415) (<= x 585)) (set-coords-x! currentCoords (- x 300)) (if (and (>= x 615) (<= x 785)) (set-coords-x! currentCoords (- x 700)) (set-coords-x! currentCoords (- x 1100)))))
                          ((equal? (length (temp-lst tmp)) 4)
                           (if (and (>= x 315) (<= x 485)) (set-coords-x! currentCoords (- x 100)) (if (and (>= x 515) (<= x 685)) (set-coords-x! currentCoords (- x 500)) (if (and (>= x 715) (<= x 885)) (set-coords-x! currentCoords (- x 900)) (set-coords-x! currentCoords (- x 1300))))))
                          ((equal? (length (temp-lst tmp)) 5)
                           (if (and (>= x 215) (<= x 385)) (set-coords-x! currentCoords (+ x 100)) (if (and (>= x 415) (<= x 585)) (set-coords-x! currentCoords (- x 300)) (if (and (>= x 615) (<= x 785)) (set-coords-x! currentCoords (- x 700)) (if (and (>= x 815) (<= x 985)) (set-coords-x! currentCoords (- x 1100)) (set-coords-x! currentCoords (- x 1500))))))))))
(define convertCoordsReset (λ ()
                             (set-temp-lst! tmp '())))

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
            bitmapX bitmapY))
    
    (define/override (on-event event)
      (cond
        ((send event button-down? 'left) (displayln (string-append "X: " (number->string (send event get-x)) ", Y: " (number->string (send event get-y)))) (selectBoardCreature event))
        ((send event button-down? 'right) (begin
                                         (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                         (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                         (cond
                                           ((not (equal? (spellStruct-name activeSpell) "none"))
                                            (if (equal? currentTurn 1) (set-mana-currentMana! P1Mana (+ (mana-currentMana P1Mana) (send (getCard (spellStruct-name activeSpell)) get-mana))) (set-mana-currentMana! P2Mana (+ (mana-currentMana P2Mana) (send (getCard (spellStruct-name activeSpell)) get-mana))))
                                            (cancelSpell)(send hand-canvas on-paint)))))))
                                      
    (define/public selectBoardCreature
      (λ (event)
        (set-coords-y! currentCoords (send event get-y))
        (convertCoordsPrelim (coords-y currentCoords) currentTurn)
        (convertCoords (send event get-x) creaturesPlayer1 creaturesPlayer2 currentTurn (temp-lst tmp))
        (let ([y (coords-y currentCoords)])
          (let ([x (coords-x currentCoords)])
            (cond
              ((equal? currentTurn 1)
               (for ([i (length sortedObjects)])
                 (cond
                   ((< i (length sortedObjects))
                    (when (and (<= x (+ (gameObject-x (list-ref sortedObjects i)) 85)) (>= x (- (gameObject-x (list-ref sortedObjects i)) 85)))
                      (cond
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 20) (>= (gameObject-y (list-ref sortedObjects i)) -220) (<= y 20) (>= y -220))
                         (cond
                           ((not (equal? #f (index-of player1ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (cons i (rest (first indexOfSelectedGameObjects)))) (rest indexOfSelectedGameObjects)))
                            (set! selectedCreatureObjects (append (list (cons (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects))))))
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 320) (>= (gameObject-y (list-ref sortedObjects i)) 80) (<= y 320) (>= y 80))
                         (cond
                           ((not (equal? #f (index-of player2ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (cons (first (first indexOfSelectedGameObjects)) (list i))) (rest indexOfSelectedGameObjects)))
                            (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref creaturesPlayer2 (index-of player2ObjectIndex i))))) (rest selectedCreatureObjects)))))
                            (cond
                              ((not (equal? (config:player1SelectedFriendlyCreature selectedCreatureObjects) config:noCreature))
                               (attack (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) (send (config:player1SelectedEnemyCreature selectedCreatureObjects) get-card)))))))))))
              ((equal? currentTurn 2)
               (for ([i (length sortedObjects)])
                 (cond
                   ((< i (length sortedObjects))
                    (when (and (<= x (+ (gameObject-x (list-ref sortedObjects i)) 85)) (>= x (- (gameObject-x (list-ref sortedObjects i)) 85)))
                      (cond
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 20) (>= (gameObject-y (list-ref sortedObjects i)) -220) (<= y 20) (>= y -220))
                         (cond
                           ((not (equal? #f (index-of player2ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (first indexOfSelectedGameObjects)) (list (cons (first (first (rest indexOfSelectedGameObjects))) (list i)))))
                            (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref creaturesPlayer2 (index-of player2ObjectIndex i))))))))))
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 320) (>= (gameObject-y (list-ref sortedObjects i)) 80) (<= y 320) (>= y 80))
                         (cond
                           ((not (equal? #f (index-of player1ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (first indexOfSelectedGameObjects)) (list (cons i (list (second (first (rest indexOfSelectedGameObjects))))))))
                            (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) (list (second (first (rest selectedCreatureObjects))))))))))
                         (cond
                           ((not (equal? (config:player2SelectedFriendlyCreature selectedCreatureObjects) config:noCreature))
                            (attack (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) (send (config:player2SelectedEnemyCreature selectedCreatureObjects) get-card))))))))))))))
        (convertCoordsReset)))
      
                                                      
    (super-new)))

(define canvas (new bitmap-canvas% [parent frame] [bitmap logo] [bitmap-scale 1] [min-height 600]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define bottomFrame (new horizontal-panel% [parent frame] [alignment '(right bottom)]))

(new button% [parent bottomFrame]
             [label "Play Card"]
             [horiz-margin 5])
(new button% [parent bottomFrame]
             [label "Show Hand"]
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send handFrame show #t))])
(new button% [parent bottomFrame]
             [label "End Turn"]
             [horiz-margin 5]
             [callback (lambda (button event)
                         (begin (send handFrame show #f)
                                (cond
                                  ((equal? currentTurn 1)
                                   (if (not (equal? (spellStruct-name activeSpell) "none")) (cancelSpell) #f)
                                   (for ([i (length creaturesPlayer1)])
                                     (when (equal? (send (send (list-ref creaturesPlayer1 i) get-card) get-sleep) #t)
                                       (send (send (list-ref creaturesPlayer1 i) get-card) set-sleep #f)))
                                   (send btnDmgPlayer set-label "Attack Player 1"))
                                  ((equal? currentTurn 2)
                                   (if (not (equal? (spellStruct-name activeSpell) "none")) (cancelSpell) #f)
                                   (for ([i (length creaturesPlayer2)])
                                     (when (equal? (send (send (list-ref creaturesPlayer2 i) get-card) get-sleep) #t)
                                       (send (send (list-ref creaturesPlayer2 i) get-card) set-sleep #f)))
                                   (send btnDmgPlayer set-label "Attack Player 2")))
                                (for ([i (length sortedObjects)])
                                         (cond
                                           ((equal? (gameObject-y (list-ref sortedObjects i)) config:currentPlayerY)
                                            (set-gameObject-y! (list-ref sortedObjects i) config:enemyPlayerY))
                                           ((equal? (gameObject-y (list-ref sortedObjects i)) config:enemyPlayerY)
                                            (set-gameObject-y! (list-ref sortedObjects i) config:currentPlayerY))))
                                (endTurn manaDisplay playerDisplay) (refreshBoard)))])

(define btnDmgPlayer (new button% [parent frame] [label "Attack Player 2"]
                          [callback (λ (button event)
                                      (cond
                                        ((equal? currentTurn 1)
                                         (if (not (equal? (config:player1SelectedFriendlyCreature selectedCreatureObjects) 10))
                                             (if (equal? #f (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) get-sleep))
                                                 (begin (playerDamage (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) get-attack) currentTurn)
                                                        (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) set-sleep #t)) #f) #f))
                                        ((equal? currentTurn 2)
                                         (if (not (equal? (config:player2SelectedFriendlyCreature selectedCreatureObjects) 10))
                                             (if (equal? #f (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) get-sleep))
                                                 (begin (playerDamage (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) get-attack) currentTurn)
                                                        (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) set-sleep #t)) #f) #f)))
                                      (if (>= 0 P1Health) (begin (for ([i (length sortedObjects)])
                                                                   (when (or (equal? 0 (gameObject-layer (list-ref sortedObjects i)))(equal? 1 (gameObject-layer (list-ref sortedObjects i))))
                                                                     (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                                                       (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                                                                 (for ([i (length (temp-lst tmp))])
                                                                   (removeObject (list-ref (temp-lst tmp) i))) (P2winMsg)) #f)
                                      (if (>= 0 P2Health) (begin (for ([i (length sortedObjects)])
                                                                   (when (or (equal? 0 (gameObject-layer (list-ref sortedObjects i)))(equal? 1 (gameObject-layer (list-ref sortedObjects i))))
                                                                     (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                                                       (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                                                                 (for ([i (length (temp-lst tmp))])
                                                                   (removeObject (list-ref (temp-lst tmp) i))) (P1winMsg)) #f))]))
  
(define manaDisplay (new message% [parent frame] [label (string-append "mana:" (number->string (mana-currentMana P1Mana)) "/" (number->string (mana-manaCap P1Mana)))]))
(define playerDisplay (new message% [parent frame] [label (string-append "Player: " (number->string currentTurn) "'s turn.")]))
(define P1winMsg (λ () (addObject (htdp:text "Player 1 Wins!" 140 "black") 0 100 0.5 1)))
(define P2winMsg (λ () (addObject (htdp:text "Player 2 Wins!" 140 "black") 0 100 0.5 1)))

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
                   (send frame show #t)))

(define greenUnderlay (htdp:rectangle 250 380 "solid" "green"))

(define redUnderlay (htdp:rectangle 250 380 "solid" "red"))

(define hitEffect (htdp:rotate 10 (htdp:scale/xy 3 4 (htdp:overlay (htdp:star-polygon 15 10 3 "solid" "red")
                                (htdp:star-polygon 20 10 3 "solid" "yellow")  
                                (htdp:star-polygon 20 10 3 "outline" "black")
                                ))))

;(addObject (htdp:bitmap/file "image.jpg") 0 200 0.7 1)
;(addObject greenUnderlay 200 0 1 2)
;(addObject redUnderlay 200 0 1 0)
;(addObject (htdp:bitmap/file "image.jpg") 0 -100 0.7 1)

(define handFrame (new frame%
                   [label "Window"] [width 1400] [height 400]
                   [style '(no-resize-border)]))

(define hand-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (init-field [bitmapX 0])
    (init-field [bitmapY 0])
    (init-field [bitmap-scale 1])
    (inherit get-dc)
    (define/override (on-paint)
      (cond
        ((equal? currentTurn 1)
         (cond
           ((not (= 0 (length P1hand)))
            (send (get-dc) draw-bitmap
                  (image->bitmap (htdp:scale bitmap-scale (htdp:overlay (htdp:scale 0.7 (generateHandImage (send (first P1hand) get-image))) (htdp:bitmap/file "Images/Other/bg_hand.jpg"))))
                  bitmapX bitmapY))
           (#t (send (get-dc) draw-bitmap (image->bitmap (htdp:bitmap/file "Images/Other/bg_hand.jpg")) bitmapX bitmapY))))
        ((equal? currentTurn 2)
         (cond
           ((not (= 0 (length P2hand)))
            (send (get-dc) draw-bitmap
                  (image->bitmap (htdp:scale bitmap-scale (htdp:overlay (htdp:scale 0.7 (generateHandImage (send (first P2hand) get-image))) (htdp:bitmap/file "Images/Other/bg_hand.jpg"))))
                  bitmapX bitmapY))
           (#t (send (get-dc) draw-bitmap (image->bitmap (htdp:bitmap/file "Images/Other/bg_hand.jpg")) bitmapX bitmapY))))))
    
    (define/public (playClickedCard event)
      (let ([x (send event get-x)])
        (let ([y (send event get-y)])
          (cond
            ((equal? currentTurn 1)
                     (cond
                       ((and (< y 25) (> y 370)) #f)
                       ((equal? (length P1hand) 0) #f)
                       ((equal? (length P1hand) 1)
                        (cond ((and (>= x (first (first config:oneCardHandRange))) (<= x (second (first config:oneCardHandRange)))) (playCard 0)))) 
                       ((equal? (length P1hand) 2)
                        (for ([i (length config:twoCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:twoCardHandRange i))) (<= x (second (list-ref config:twoCardHandRange i)))) (playCard i)))))
                       ((equal? (length P1hand) 3)
                        (for ([i (length config:threeCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:threeCardHandRange i))) (<= x (second (list-ref config:threeCardHandRange i))) (playCard i))))))
                       ((equal? (length P1hand) 4)
                        (for ([i (length config:fourCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:fourCardHandRange i))) (<= x (second (list-ref config:fourCardHandRange i))) (playCard i)))))) 
                       ((equal? (length P1hand) 5)
                        (for ([i (length config:fiveCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:fiveCardHandRange i))) (<= x (second (list-ref config:fiveCardHandRange i))) (playCard i))))))))
            ((equal? currentTurn 2)
                     (cond
                       ((and (< y 25) (> y 370)) #f)
                       ((equal? (length P2hand) 0) #f)
                       ((equal? (length P2hand) 1)
                        (cond ((and (>= x (first (first config:oneCardHandRange))) (<= x (second (first config:oneCardHandRange)))) (playCard 0)))) 
                       ((equal? (length P2hand) 2)
                        (for ([i (length config:twoCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:twoCardHandRange i))) (<= x (second (list-ref config:twoCardHandRange i)))) (playCard i)))))
                       ((equal? (length P2hand) 3)
                        (for ([i (length config:threeCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:threeCardHandRange i))) (<= x (second (list-ref config:threeCardHandRange i))) (playCard i))))))
                       ((equal? (length P2hand) 4)
                        (for ([i (length config:fourCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:fourCardHandRange i))) (<= x (second (list-ref config:fourCardHandRange i))) (playCard i)))))) 
                       ((equal? (length P2hand) 5)
                        (for ([i (length config:fiveCardHandRange)])
                          (cond ((and (>= x (first (list-ref config:fiveCardHandRange i))) (<= x (second (list-ref config:fiveCardHandRange i))) (playCard i)))))) 
                       ))))))
    (define/override (on-event event)
      (cond
        ((send event button-down? 'left) (playClickedCard event))))
    (super-new)))

(define hand-canvas (new hand-canvas% [parent handFrame] [bitmap logo] [bitmap-scale 1] [min-height 400]))

(define generateHandImage (λ (image [i 1])
                            (cond
                              ((equal? currentTurn 1)
                               (cond
                                 ((= (length P1hand) i) image)
                                 (#t (generateHandImage (htdp:beside image (send (list-ref P1hand i) get-image)) (+ i 1)))))
                              ((equal? currentTurn 2)
                               (cond
                                 ((= (length P2hand) i) image)
                                 (#t (generateHandImage (htdp:beside image (send (list-ref P2hand i) get-image)) (+ i 1)))
                                 )))))
(define sortPlayer1ObjectIndex (λ ()
                                 (set! player1ObjectIndex '())
                                 (cond
                                   ((equal? currentTurn 1)
                                    (for ([i (length sortedObjects)])
                                      (when (equal? (gameObject-y (list-ref sortedObjects i)) config:currentPlayerY)
                                        (set! player1ObjectIndex (append player1ObjectIndex (list i))))))
                                   ((equal? currentTurn 2)
                                    (for ([i (length sortedObjects)])
                                      (when (equal? (gameObject-y (list-ref sortedObjects i)) config:enemyPlayerY)
                                        (set! player1ObjectIndex (append player1ObjectIndex (list i)))))))
                                    (set! player1ObjectIndex (removed2 player1ObjectIndex))))
                                    
(define sortPlayer2ObjectIndex (λ ()
                                 (set! player2ObjectIndex '())
                                 (cond
                                   ((equal? currentTurn 1)
                                    (for ([i (length sortedObjects)])
                                      (when (equal? (gameObject-y (list-ref sortedObjects i)) config:enemyPlayerY)
                                        (set! player2ObjectIndex (append player2ObjectIndex (list i))))))
                                   ((equal? currentTurn 2)
                                    (for ([i (length sortedObjects)])
                                      (when (equal? (gameObject-y (list-ref sortedObjects i)) config:currentPlayerY)
                                        (set! player2ObjectIndex (append player2ObjectIndex (list i)))))))
                                    (set! player2ObjectIndex (removed2 player2ObjectIndex))))
(define refreshBoard (λ ()
                       (sortPlayer1ObjectIndex)
                       (cond
                         ((equal? (length creaturesPlayer1) 1)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 0)
                          (send canvas on-paint)
                          (send canvas show #t))
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
                          (send canvas show #t)))
                       (sortPlayer2ObjectIndex)
                       (cond
                         ((equal? (length creaturesPlayer2) 1)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 0)
                          (send canvas on-paint)
                          (send canvas show #t))
                         ((equal? (length creaturesPlayer2) 2)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) -100)
                          (send canvas on-paint)
                          (send canvas show #t))
                         ((equal? (length creaturesPlayer2) 3)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) -200)
                          (send canvas on-paint)
                          (send canvas show #t))
                         ((equal? (length creaturesPlayer2) 4)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 300)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) -100)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player2ObjectIndex)) -300)
                          (send canvas on-paint)
                          (send canvas show #t))
                         ((equal? (length creaturesPlayer2) 5)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 400)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player2ObjectIndex)) -200)
                          (set-gameObject-x! (list-ref sortedObjects (fifth player2ObjectIndex)) -400)
                          (send canvas on-paint)
                          (send canvas show #t)))
                       (for ([i (length sortedObjects)])
                         (cond
                           ((not (equal? (length creaturesPlayer1) 0))
                            (if (not (equal? #f (index-of player1ObjectIndex i)))
                                (cond
                                  ((and (is-a? (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) creatureObject%) (not (equal? (send (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) get-lifeDisplay) "none")))
                                   (displayCreatureLife (list-ref sortedObjects i) (send (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) get-card) (list-ref creaturesPlayer1 (index-of player1ObjectIndex i))))) #f)))
                         (cond
                           ((not (equal? (length creaturesPlayer2) 0))
                            (if (not (equal? #f (index-of player2ObjectIndex i)))
                                (cond
                                  ((and (is-a? (list-ref creaturesPlayer2 (index-of player2ObjectIndex i)) creatureObject%) (not (equal? (send (list-ref creaturesPlayer2 (index-of player2ObjectIndex i)) get-lifeDisplay) "none")))
                                   (displayCreatureLife (list-ref sortedObjects i) (send (list-ref creaturesPlayer2 (index-of player2ObjectIndex i)) get-card) (list-ref creaturesPlayer2 (index-of player2ObjectIndex i))))) #f))))))
(define playCard (λ (pos)
                   (cond
                     ((equal? currentTurn 1)
                      (let ([cardMana (send (list-ref P1hand pos) get-mana)])
                        (cond
                          ((>= (mana-currentMana P1Mana) cardMana)
                           (cond
                             ((is-a? (list-ref P1hand pos) spell%)
                              (set-spellStruct-name! activeSpell (send (list-ref P1hand pos) get-name))
                              (set-mana-currentMana! P1Mana (- (mana-currentMana P1Mana) cardMana))
                              (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P1Mana)) "/" (number->string (mana-manaCap P1Mana))))
                              (removeCardFromHand pos)
                              (send hand-canvas on-paint)
                              (send hand-canvas show #t))
                             ((is-a? (list-ref P1hand pos) creature%)
                              (let ([x (packageCardObject (list-ref P1hand pos) pos)])
                                (set! creatureObjects (append creatureObjects (list x)))
                                (set! creaturesPlayer1 (append creaturesPlayer1 (list x)))
                                (send x set-index (index-of creaturesPlayer1 x))
                                (send (send x get-card) set-sleep #t)
                                (addObject (send (send x get-card) get-image) 0 config:currentPlayerY 0.5 1)
                                (removeCardFromHand pos)
                                (set-mana-currentMana! P1Mana (- (mana-currentMana P1Mana) cardMana))
                                (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P1Mana)) "/" (number->string (mana-manaCap P1Mana))))
                                (send hand-canvas on-paint)
                                (send hand-canvas show #t)
                                (refreshBoard)))))
                          ((< (mana-currentMana P1Mana) cardMana) #f))))
                      ((equal? currentTurn 2)
                       (let ([cardMana (send (list-ref P2hand pos) get-mana)])
                        (cond
                          ((>= (mana-currentMana P2Mana) cardMana)
                           (cond
                             ((is-a? (list-ref P2hand pos) spell%)
                              (set-spellStruct-name! activeSpell (send (list-ref P2hand pos) get-name))
                              (set-mana-currentMana! P2Mana (- (mana-currentMana P2Mana) cardMana))
                              (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P2Mana)) "/" (number->string (mana-manaCap P2Mana))))
                              (removeCardFromHand pos)
                              (send hand-canvas on-paint)
                              (send hand-canvas show #t))
                             ((is-a? (list-ref P2hand pos) creature%)
                              (let ([x (packageCardObject (list-ref P2hand pos) pos)])
                                (set! creatureObjects (append creatureObjects (list x)))
                                (set! creaturesPlayer2 (append creaturesPlayer2 (list x)))
                                (send x set-index (index-of creaturesPlayer2 x))
                                (addObject (send (send x get-card) get-image) 0 config:currentPlayerY 0.5 1)
                                (removeCardFromHand pos)
                                (set-mana-currentMana! P2Mana (- (mana-currentMana P2Mana) cardMana))
                                (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P2Mana)) "/" (number->string (mana-manaCap P2Mana))))
                                (send hand-canvas on-paint)
                                (send hand-canvas show #t)
                                (refreshBoard)))))
                          ((< (mana-currentMana P2Mana) cardMana) #f))))
                           )))

(define attack (λ (attacker defender)
              (cond
                ((equal? (send attacker get-sleep) #f)
                    (cond
                      ((equal? currentTurn 1)
                       (if (>= (send attacker get-attack) (send defender get-life))
                           (begin (removeObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                                  (set! creaturesPlayer2 (remove (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer2))
                                  (removeCreatureObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                                  (removeCreatureLifeDisplay (config:player1SelectedEnemyCreature selectedCreatureObjects)))
                           (begin (send defender set-life (- (send defender get-life) (send attacker get-attack)))
                                  (displayCreatureLife (list-ref sortedObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) defender (second (first selectedCreatureObjects)))
                                  ))
                       (if (>= (send defender get-attack) (send attacker get-life))
                           (begin (removeObject (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects))
                                  (set! creaturesPlayer1 (remove (list-ref creatureObjects (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects)) creaturesPlayer1))
                                  (removeCreatureObject (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects))
                                  (removeCreatureLifeDisplay (config:player1SelectedFriendlyCreature selectedCreatureObjects)))
                           (begin (send attacker set-life (- (send attacker get-life) (send defender get-attack)))
                                  (displayCreatureLife (list-ref sortedObjects (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects)) attacker (config:player1SelectedFriendlyCreature selectedCreatureObjects))))
                       (refreshBoard)
                       (send attacker set-sleep #t)
                       (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                       (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))
                      ((equal? currentTurn 2)
                       (if (>= (send attacker get-attack) (send defender get-life))
                           (begin (removeObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                                  (set! creaturesPlayer1 (remove (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer1))
                                  (removeCreatureObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                                  (removeCreatureLifeDisplay (config:player2SelectedEnemyCreature selectedCreatureObjects)))
                           (begin (send defender set-life (- (send defender get-life) (send attacker get-attack)))
                                  (displayCreatureLife (list-ref sortedObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) defender (config:player2SelectedEnemyCreature selectedCreatureObjects))))
                       (if (>= (send defender get-attack) (send attacker get-life))
                           (begin (removeObject (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects))
                                  (set! creaturesPlayer2 (remove (list-ref creatureObjects (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects)) creaturesPlayer2))
                                  (removeCreatureObject (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects))
                                  (removeCreatureLifeDisplay (config:player2SelectedFriendlyCreature selectedCreatureObjects)))
                           (begin (send attacker set-life (- (send attacker get-life) (send defender get-attack)))
                                  (displayCreatureLife (list-ref sortedObjects (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects)) attacker (config:player2SelectedFriendlyCreature selectedCreatureObjects))))
                       (refreshBoard)
                       (send attacker set-sleep #t)
                       (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                       (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))))
                   ((equal? (send attacker get-sleep) #t) #f))))

(define displayCreatureLife (λ (gameObj creature creatureObj)
                                 (removeCreatureLifeDisplay creatureObj)
                                 (send creatureObj set-lifeDisplay (gameObject (htdp:text (number->string (send creature get-life)) 80 "red") (- (gameObject-x gameObj) 68) (- (gameObject-y gameObj) 100) 0.5 2))
                                 (addObject (htdp:text (number->string (send creature get-life)) 80 "red") (gameObject-x (send creatureObj get-lifeDisplay)) (gameObject-y (send creatureObj get-lifeDisplay)) (gameObject-scale (send creatureObj get-lifeDisplay)) (gameObject-layer (send creatureObj get-lifeDisplay)))
                                 (send canvas on-paint)(send canvas show #t)))
(provide startGUI
         addObject)