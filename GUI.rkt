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

(define objects '())
(define sortedObjects '())
(define player1ObjectIndex '())
(define player2ObjectIndex '())
(define creaturesPlayer1 '())
(define creaturesPlayer2 '())
(define creatureObjects '())

(define indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
(define selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))

(define addObject (λ (image x y scale layer [player 3])
                    (set! objects (append objects (list (gameObject image x y scale layer player))))
                    (send canvas on-paint)))

(define removeObject (λ (index)
                       (set! sortedObjects (remove (list-ref sortedObjects index) sortedObjects))
                       (set! objects sortedObjects)))

(define removeCreatureObject (λ (index)
                       (set! creatureObjects (remove (list-ref creatureObjects index) creatureObjects))))

(define removeCreatureLifeDisplayResult '())
(define removeCreatureLifeDisplay (λ ()
                                    (for ([i (length sortedObjects)])
                                      (when (and (equal? (gameObject-layer (list-ref sortedObjects i)) 2)(equal? (gameObject-player (list-ref sortedObjects i)) 3))
                                        (set! removeCreatureLifeDisplayResult (append removeCreatureLifeDisplayResult (list (- i (length removeCreatureLifeDisplayResult)))))))
                                    (for ([i (length removeCreatureLifeDisplayResult)])
                                      (removeObject (list-ref removeCreatureLifeDisplayResult i)))
                                    (set! removeCreatureLifeDisplayResult '())))
(define sortObjects (λ ([count 0] [res '()]) (cond
                                     ((= count 10) res)
                                     (#t (let ([x '()])
                                           (for ([i (length objects)])
                                             (when (= count (gameObject-layer (list-ref objects i))) (set! x (append x (list (list-ref objects i))))))
                                           (sortObjects (+ count 1) (append res x)))))))
(define sortObjectsPhase2 (λ ([count 0] [res '()]) (cond
                                     ((= count 10) res)
                                     (#t (let ([x '()])
                                           (for ([i (length sortedObjects)])
                                             (when (= count (gameObject-player (list-ref objects i))) (set! x (append x (list (list-ref sortedObjects i))))))
                                           (sortObjectsPhase2 (+ count 1) (append res x)))))))    
(define sortCreatureObjects (λ ([count 0] [res '()]) (cond
                                     ((= count 3) res)
                                     (#t (let ([x '()])
                                           (for ([i (length creatureObjects)])
                                             (when (= count (send (list-ref creatureObjects i) get-player)) (set! x (append x (list (list-ref creatureObjects i))))))
                                           (sortCreatureObjects (+ count 1) (append res x)))))))
(define generateGameScene (λ () 
                            (set! sortedObjects (sortObjects))
                            (set! sortedObjects (sortObjectsPhase2))
                            (set! creatureObjects (sortCreatureObjects))
                            (sortPlayer1ObjectIndex)
                            (sortPlayer2ObjectIndex)
                            (let ([bg (htdp:bitmap/file "Images/Other/bg.jpg")])
                              (let ([scene (overlay (htdp:overlay/offset (htdp:bitmap/file "Images/Other/divider.png") 0 50 bg))])
                            
                            (for ([i (length creaturesPlayer1)])
                              (when (send (send (list-ref creaturesPlayer1 i) get-card) get-sleep) (begin
                                                                                                     (set! scene (htdp:overlay/offset (htdp:bitmap/file "Images/Other/sleep.png") (gameObject-x (gameObject-of-index 1 i)) (gameObject-y (gameObject-of-index 1 i)) scene))
                                                                                                     )
                              ))
                            (for ([i (length creaturesPlayer2)])
                              (when (send (send (list-ref creaturesPlayer2 i) get-card) get-sleep) (begin
                                                                                                     (set! scene (htdp:overlay/offset (htdp:bitmap/file "Images/Other/sleep.png") (gameObject-x (gameObject-of-index 2 i)) (gameObject-y (gameObject-of-index 2 i)) scene))
                                                                                                     )
                             ))
                            scene
                            ))))
                            
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
                           (if (and (>= x 515) (<= x 685)) (set-coords-x! currentCoords (- x 500)) (if (and (>= x 715) (<= x 885)) (set-coords-x! currentCoords (- x 900)) (set-coords-x! currentCoords 4000))))
                          ((equal? (length (temp-lst tmp)) 3)
                           (if (and (>= x 415) (<= x 585)) (set-coords-x! currentCoords (- x 300)) (if (and (>= x 615) (<= x 785)) (set-coords-x! currentCoords (- x 700)) (if (and (>= x 815) (<= x 985)) (set-coords-x! currentCoords (- x 1100)) (set-coords-x! currentCoords 4000)))))
                          ((equal? (length (temp-lst tmp)) 4)
                           (if (and (>= x 315) (<= x 485)) (set-coords-x! currentCoords (- x 100)) (if (and (>= x 515) (<= x 685)) (set-coords-x! currentCoords (- x 500)) (if (and (>= x 715) (<= x 885)) (set-coords-x! currentCoords (- x 900)) (if (and (>= x 915) (<= x 1085)) (set-coords-x! currentCoords (- x 1300)) (set-coords-x! currentCoords 4000))))))
                          ((equal? (length (temp-lst tmp)) 5)
                           (if (and (>= x 215) (<= x 385)) (set-coords-x! currentCoords (+ x 100)) (if (and (>= x 415) (<= x 585)) (set-coords-x! currentCoords (- x 300)) (if (and (>= x 615) (<= x 785)) (set-coords-x! currentCoords (- x 700)) (if (and (>= x 815) (<= x 985)) (set-coords-x! currentCoords (- x 1100)) (if  (and (>= x 1015) (<= x 1185)) (set-coords-x! currentCoords (- x 1500)) (set-coords-x! currentCoords 4000))))))))))
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
                                           ((not (equal? (config:spellStruct-name config:activeSpell) "none"))
                                            (if (equal? currentTurn 1) (set-mana-currentMana! P1Mana (+ (mana-currentMana P1Mana) (send (getCard (config:spellStruct-name config:activeSpell)) get-mana))) (set-mana-currentMana! P2Mana (+ (mana-currentMana P2Mana) (send (getCard (config:spellStruct-name config:activeSpell)) get-mana))))
                                            (cancelSpell)(send hand-canvas on-paint)))))))
                                      
    (define/public selectBoardCreature
      (λ (event)
        (set-coords-y! currentCoords (send event get-y))
        (convertCoordsPrelim (coords-y currentCoords) currentTurn)
        (convertCoords (send event get-x) creaturesPlayer1 creaturesPlayer2 currentTurn (temp-lst tmp))
        (set-temp-lst! tmp '())
        (for ([i (length sortedObjects)])
          (when (equal? (gameObject-layer (list-ref sortedObjects i)) 4)
            (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp))))))))
        (for ([i (length (temp-lst tmp))])
          (removeObject (list-ref (temp-lst tmp) i)))
        (set-temp-lst! tmp '())
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
                            (set! selectedCreatureObjects (append (list (cons (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                            (addObject (htdp:text "*" 75 "red") (- (gameObject-x (list-ref sortedObjects (first (first indexOfSelectedGameObjects)))) 60) (+ (gameObject-y (list-ref sortedObjects (first (first indexOfSelectedGameObjects)))) 102) 0.5 4))))
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 320) (>= (gameObject-y (list-ref sortedObjects i)) 80) (<= y 320) (>= y 80))
                         (cond
                           ((not (equal? #f (index-of player2ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (cons (first (first indexOfSelectedGameObjects)) (list i))) (rest indexOfSelectedGameObjects)))
                            (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref creaturesPlayer2 (index-of player2ObjectIndex i))))) (rest selectedCreatureObjects)))))
                         (cond
                           ((not (equal? (config:spellStruct-effect config:activeSpell) "none"))
                            ((config:spellStruct-effect config:activeSpell) (send (config:player1SelectedEnemyCreature selectedCreatureObjects) get-card) (config:spellStruct-num config:activeSpell))
                             (displayCreatureLife (list-ref sortedObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) (send (config:player1SelectedEnemyCreature selectedCreatureObjects) get-card) (config:player1SelectedEnemyCreature selectedCreatureObjects))
                             (config:set-spellStruct-effect! config:activeSpell "none") (config:set-spellStruct-name! config:activeSpell "none") (config:set-spellStruct-num! config:activeSpell "none")
                             (if (>= 0 (send (send (config:player1SelectedEnemyCreature selectedCreatureObjects) get-card) get-life))
                                 (begin
                                   (cond
                                     ((equal? (send (send (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-name) "Cultist")
                                      (P2drawCard))
                                     ((not (equal? (send (send (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-death-effect) "none"))
                                      ((send (send (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-death-effect) 2)
                                      ))
                                   (removeObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                                   (set! creaturesPlayer2 (remove (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer2))
                                   (removeCreatureObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                                   (displayCreatureLife)
                                   (set! selectedCreatureObjects (list (list 10 10) (list 10 10))) (set! indexOfSelectedGameObjects (list (list 10 10) (list 10 10))))
                                 (begin
                                   (set! selectedCreatureObjects (list (list 10 10) (list 10 10)))
                                   (set! indexOfSelectedGameObjects (list (list 10 10) (list 10 10))))))
                           ((not (equal? (config:player1SelectedFriendlyCreature selectedCreatureObjects) config:noCreature))
                            (set-temp-lst! tmp sortedObjects)
                            (attack (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) (send (config:player1SelectedEnemyCreature selectedCreatureObjects) get-card))
                              )))))))))
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
                            (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref creaturesPlayer2 (index-of player2ObjectIndex i)))))))
                              (when (equal? (gameObject-layer (list-ref sortedObjects i)) 4)
                                (removeObject i))
                            (addObject (htdp:text "*" 75 "red") (- (gameObject-x (list-ref sortedObjects (second (second indexOfSelectedGameObjects)))) 60) (+ (gameObject-y (list-ref sortedObjects (second (second indexOfSelectedGameObjects)))) 102) 0.5 4))))
                        ((and (<= (gameObject-y (list-ref sortedObjects i)) 320) (>= (gameObject-y (list-ref sortedObjects i)) 80) (<= y 320) (>= y 80))
                         (cond
                           ((not (equal? #f (index-of player1ObjectIndex i)))
                            (set! indexOfSelectedGameObjects (append (list (first indexOfSelectedGameObjects)) (list (cons i (list (second (first (rest indexOfSelectedGameObjects))))))))
                            (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref creaturesPlayer1 (index-of player1ObjectIndex i)) (list (second (first (rest selectedCreatureObjects))))))))))
                         (cond
                           ((not (equal? (config:spellStruct-effect config:activeSpell) "none"))
                            ((config:spellStruct-effect config:activeSpell) (send (config:player2SelectedEnemyCreature selectedCreatureObjects) get-card) (config:spellStruct-num config:activeSpell))
                             (displayCreatureLife (list-ref sortedObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) (send (config:player2SelectedEnemyCreature selectedCreatureObjects) get-card) (config:player2SelectedEnemyCreature selectedCreatureObjects))
                             (config:set-spellStruct-effect! config:activeSpell "none") (config:set-spellStruct-name! config:activeSpell "none") (config:set-spellStruct-num! config:activeSpell "none")
                             (if (>= 0 (send (send (config:player2SelectedEnemyCreature selectedCreatureObjects) get-card) get-life))
                                 (begin
                                   (cond
                                     ((equal? (send (send (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-name) "Cultist")
                                      (P1drawCard))
                                     ((not (equal? (send (send (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-death-effect) "none"))
                                      ((send (send (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) get-card) get-death-effect) 1)
                                      ))
                                   (removeObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                                   (set! creaturesPlayer1 (remove (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer1))
                                   (removeCreatureObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                                   (displayCreatureLife)
                                   (set! selectedCreatureObjects (list (list 10 10) (list 10 10))) (set! indexOfSelectedGameObjects (list (list 10 10) (list 10 10))))
                                 (begin
                                   (set! selectedCreatureObjects (list (list 10 10) (list 10 10)))
                                   (set! indexOfSelectedGameObjects (list (list 10 10) (list 10 10))))))
                           ((not (equal? (config:player2SelectedFriendlyCreature selectedCreatureObjects) config:noCreature))
                            (set-temp-lst! tmp sortedObjects)
                            (attack (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) (send (config:player2SelectedEnemyCreature selectedCreatureObjects) get-card))
                            ))))))))))
            (set-temp-lst! tmp '())
            (for ([i (length sortedObjects)])
              (when (equal? (gameObject-layer (list-ref sortedObjects i)) 4)
                (set-temp-lst! tmp (append (temp-lst tmp) (list i)))))
            (cond
              ((equal? currentTurn 1)
               (cond
                 ((not (equal? (first (first indexOfSelectedGameObjects)) 10))
                  (cond
                    ((equal? (length (temp-lst tmp)) 0)
                     (addObject (htdp:text "*" 75 "red") (- (gameObject-x (list-ref sortedObjects (first (first indexOfSelectedGameObjects)))) 60) (+ (gameObject-y (list-ref sortedObjects (first (first indexOfSelectedGameObjects)))) 102) 0.5 4))))))
              ((equal? currentTurn 2)
               (cond
                 ((not (equal? (second (second indexOfSelectedGameObjects)) 10))
                  (cond
                    ((equal? (length (temp-lst tmp)) 0)
                     (addObject (htdp:text "*" 75 "red") (- (gameObject-x (list-ref sortedObjects (second (second indexOfSelectedGameObjects)))) 60) (+ (gameObject-y (list-ref sortedObjects (second (second indexOfSelectedGameObjects)))) 102) 0.5 4)))))))
               ))
        (refreshBoard)(convertCoordsReset)))
      
                                                      
    (super-new)))

(define canvas (new bitmap-canvas% [parent frame] [bitmap logo] [bitmap-scale 1] [min-height 600]))

(define bottomFrame (new horizontal-panel% [parent frame] [alignment '(center bottom)]))
(define bottomLeftFrame (new horizontal-panel% [parent frame] [alignment '(right bottom)]))
(new button% [parent bottomLeftFrame]
             [label "Play Card"]
             [horiz-margin 5])
(new button% [parent bottomLeftFrame]
             [label "Show Hand"]
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send handFrame show #t))])
(new button% [parent bottomLeftFrame]
             [label "End Turn"]
             [horiz-margin 5]
             [callback (lambda (button event)
                         (begin (send handFrame show #f)
                                (cond
                                  ((equal? currentTurn 1)
                                   (if (not (equal? (config:spellStruct-effect config:activeSpell) "none")) (cancelSpell) #f)
                                   (for ([i (length creaturesPlayer1)])
                                     (when (equal? (send (send (list-ref creaturesPlayer1 i) get-card) get-sleep) #t)
                                       (send (send (list-ref creaturesPlayer1 i) get-card) set-sleep #f)))
                                   (send btnDmgPlayer set-label "Attack Player 1"))
                                  ((equal? currentTurn 2)
                                   (if (not (equal? (config:spellStruct-effect config:activeSpell) "none")) (cancelSpell) #f)
                                   (for ([i (length creaturesPlayer2)])
                                     (when (equal? (send (send (list-ref creaturesPlayer2 i) get-card) get-sleep) #t)
                                       (send (send (list-ref creaturesPlayer2 i) get-card) set-sleep #f)))
                                   (send btnDmgPlayer set-label "Attack Player 2")))
                                (for ([i (length sortedObjects)])
                                         (cond
                                           ((equal? (gameObject-y (list-ref sortedObjects i)) config:currentPlayerY)
                                            (set-gameObject-y! (list-ref sortedObjects i) config:enemyPlayerY))
                                           ((equal? (gameObject-y (list-ref sortedObjects i)) config:enemyPlayerY)
                                            (set-gameObject-y! (list-ref sortedObjects i) config:currentPlayerY)))
                                  (when (and (equal? (gameObject-layer (list-ref sortedObjects i)) 4)(equal? (gameObject-player (list-ref sortedObjects i)) 3))
                                        (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp))))))))
                                (for ([i (length (temp-lst tmp))])
                                  (removeObject (list-ref (temp-lst tmp) i)))
                                (set-temp-lst! tmp '())
                                (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                (endTurn manaDisplay playerDisplay) (refreshBoard)))])

(define btnDmgPlayer (new button% [parent frame] [label "Attack Player 2"]
                          [callback (λ (button event)
                                      (cond
                                        ((equal? currentTurn 1)
                                         (if (not (equal? (config:player1SelectedFriendlyCreature selectedCreatureObjects) 10))
                                             (if (equal? #f (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) get-sleep))
                                                 (begin (config:playerDamage (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) get-attack) currentTurn)
                                                        (send (send (config:player1SelectedFriendlyCreature selectedCreatureObjects) get-card) set-sleep #t)
                                                        (send player2LifeDisplay set-label (string-append "P2 Health: " (number->string config:P2Health)))) #f) #f))
                                        ((equal? currentTurn 2)
                                         (if (not (equal? (config:player2SelectedFriendlyCreature selectedCreatureObjects) 10))
                                             (if (equal? #f (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) get-sleep))
                                                 (begin (config:playerDamage (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) get-attack) currentTurn)
                                                        (send (send (config:player2SelectedFriendlyCreature selectedCreatureObjects) get-card) set-sleep #t)
                                                        (send player1LifeDisplay set-label (string-append "P1 Health: " (number->string config:P1Health)))) #f) #f)))
                                      (if (>= 0 config:P1Health) (begin (for ([i (length sortedObjects)])
                                                                   (when (or (equal? 1 (gameObject-layer (list-ref sortedObjects i)))(equal? 2 (gameObject-layer (list-ref sortedObjects i))))
                                                                     (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                                                       (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                                                                 (for ([i (length (temp-lst tmp))])
                                                                   (removeObject (list-ref (temp-lst tmp) i))) (P2winMsg)) #f)
                                      (if (>= 0 config:P2Health) (begin (for ([i (length sortedObjects)])
                                                                   (when (or (equal? 1 (gameObject-layer (list-ref sortedObjects i)))(equal? 2 (gameObject-layer (list-ref sortedObjects i))))
                                                                     (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                                                       (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                                                                 (for ([i (length (temp-lst tmp))])
                                                                   (removeObject (list-ref (temp-lst tmp) i))) (P1winMsg)) #f)
                                      (send canvas on-paint)(send canvas show #t))]))
  
(define manaDisplay (new message% [parent frame] [label (string-append "mana:" (number->string (mana-currentMana P1Mana)) "/" (number->string (mana-manaCap P1Mana)))]))
(define playerDisplay (new message% [parent frame] [label (string-append "Player: " (number->string currentTurn) "'s turn.")]))
(define player1LifeDisplay (new message% [parent bottomFrame] [label (string-append "P1 Health: " (number->string config:P1Health))] [horiz-margin 200]))
(define player2LifeDisplay (new message% [parent bottomFrame] [label (string-append "P2 Health: " (number->string config:P2Health))] [horiz-margin 200]))
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

(define l (list 10))
(define choice1 (new menu-item%
                     [parent menu-game] [label "New Game"] [callback (lambda (button event)
                         (set! l l))]))
(define documentation (new menu-item%
                     [parent menu-about] [label "Documentation"] [callback (lambda (button event)
                         (set! l l))]))

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
                                 (for ([i (length sortedObjects)])
                                   (when (equal? (gameObject-player (list-ref sortedObjects i)) 1)
                                     (set! player1ObjectIndex (append player1ObjectIndex (list i)))))
                                 (set! player1ObjectIndex (removed2 player1ObjectIndex))))
                                  
                                    
(define sortPlayer2ObjectIndex (λ ()
                                 (set! player2ObjectIndex '())
                                 (for ([i (length sortedObjects)])
                                   (when (equal? (gameObject-player (list-ref sortedObjects i)) 2)
                                     (set! player2ObjectIndex (append player2ObjectIndex (list i)))))
                                 (set! player2ObjectIndex (removed2 player2ObjectIndex))))
(define refreshBoard (λ ()
                       (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))(sortPlayer1ObjectIndex)(sortPlayer2ObjectIndex)
                       (cond
                         ((equal? (length creaturesPlayer1) 1)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 0))
                         ((equal? (length creaturesPlayer1) 2)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) -100))
                         ((equal? (length creaturesPlayer1) 3)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) -200))
                         ((equal? (length creaturesPlayer1) 4)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 300)
                          (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) -100)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player1ObjectIndex)) -300))
                         ((equal? (length creaturesPlayer1) 5)
                          (set-gameObject-x! (list-ref sortedObjects (first player1ObjectIndex)) 400)
                          (set-gameObject-x! (list-ref sortedObjects (second player1ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (third player1ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player1ObjectIndex)) -200)
                          (set-gameObject-x! (list-ref sortedObjects (fifth player1ObjectIndex)) -400)))
                       (cond
                         ((equal? (length creaturesPlayer2) 1)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 0))
                         ((equal? (length creaturesPlayer2) 2)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) -100))
                         ((equal? (length creaturesPlayer2) 3)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) -200))
                         ((equal? (length creaturesPlayer2) 4)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 300)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 100)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) -100)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player2ObjectIndex)) -300))
                         ((equal? (length creaturesPlayer2) 5)
                          (set-gameObject-x! (list-ref sortedObjects (first player2ObjectIndex)) 400)
                          (set-gameObject-x! (list-ref sortedObjects (second player2ObjectIndex)) 200)
                          (set-gameObject-x! (list-ref sortedObjects (third player2ObjectIndex)) 0)
                          (set-gameObject-x! (list-ref sortedObjects (fourth player2ObjectIndex)) -200)
                          (set-gameObject-x! (list-ref sortedObjects (fifth player2ObjectIndex)) -400)))
                       (send canvas on-paint)
                       (send canvas show #t)
                       (displayCreatureLife)))
(define playCard (λ (pos)
                   (cond
                     ((equal? currentTurn 1)
                      (let ([cardMana (send (list-ref P1hand pos) get-mana)])
                        (cond
                          ((>= (mana-currentMana P1Mana) cardMana)
                           (cond
                             ((is-a? (list-ref P1hand pos) spell%)
                              ((send (list-ref P1hand pos) get-effect))
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
                                (addObject (send (send x get-card) get-image) 0 config:currentPlayerY 0.5 1 1)
                                (removeCardFromHand pos)
                                (cond
                                  ((not (equal? (send (send x get-card) get-on-play-effect) "none"))
                                   ((send (send x get-card) get-on-play-effect) currentTurn)
                                   ))
                                (set-mana-currentMana! P1Mana (- (mana-currentMana P1Mana) cardMana))
                                (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P1Mana)) "/" (number->string (mana-manaCap P1Mana))))
                                (send player1LifeDisplay set-label (string-append "P1 Health: " (number->string config:P1Health)))
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
                              ((send (list-ref P2hand pos) get-effect))
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
                                (addObject (send (send x get-card) get-image) 0 config:currentPlayerY 0.5 1 2)
                                (removeCardFromHand pos)
                                (cond
                                  ((not (equal? (send (send x get-card) get-on-play-effect) "none"))
                                   ((send (send x get-card) get-on-play-effect) currentTurn)
                                   ))
                                (set-mana-currentMana! P2Mana (- (mana-currentMana P2Mana) cardMana))
                                (send manaDisplay set-label(string-append "mana:" (number->string (mana-currentMana P2Mana)) "/" (number->string (mana-manaCap P2Mana))))
                                (send player2LifeDisplay set-label (string-append "P2 Health: " (number->string config:P2Health)))
                                (send hand-canvas on-paint)
                                (send hand-canvas show #t)
                                (refreshBoard)))))
                          ((< (mana-currentMana P2Mana) cardMana) #f)))))
                      (if (>= 0 config:P1Health)
                          (begin
                            (for ([i (length sortedObjects)])
                              (when (or (equal? 1 (gameObject-layer (list-ref sortedObjects i)))(equal? 2 (gameObject-layer (list-ref sortedObjects i))))
                                (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                  (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                            (for ([i (length (temp-lst tmp))])
                              (removeObject (list-ref (temp-lst tmp) i))) (P2winMsg)) #f)
                      (if (>= 0 config:P2Health)
                          (begin
                            (for ([i (length sortedObjects)])
                              (when (or (equal? 1 (gameObject-layer (list-ref sortedObjects i)))(equal? 2 (gameObject-layer (list-ref sortedObjects i))))
                                (when (equal? 200 (gameObject-y (list-ref sortedObjects i)))
                                  (set-temp-lst! tmp (append (temp-lst tmp) (list (- i (length (temp-lst tmp)))))))))
                            (for ([i (length (temp-lst tmp))])
                              (removeObject (list-ref (temp-lst tmp) i))) (P1winMsg)) #f)))
(struct count (c) #:mutable)
(define creaturesRemoved (count 0))

(define attack (λ (attacker defender)
                 (for ([i (length sortedObjects)])
                   (when (equal? (gameObject-layer (list-ref sortedObjects i)) 4)
                     (removeObject i))
                   )
              (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))
              (cond
                ((equal? (send attacker get-sleep) #f)
                    (cond
                      ((equal? currentTurn 1)
                       (if (>= (send attacker get-attack) (send defender get-life))
                           (begin
                             (cond
                               ((equal? (send defender get-name) "Cultist")
                                (P2drawCard))
                               ((not (equal? (send defender get-death-effect) "none"))
                                ((send defender get-death-effect) 2)
                                ))
                             (removeObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                             (set! creaturesPlayer2 (remove (list-ref creatureObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer2))
                             (removeCreatureObject (config:player1SelectedEnemyCreature indexOfSelectedGameObjects))
                             (displayCreatureLife))
                           (begin
                             (send defender set-life (- (send defender get-life) (send attacker get-attack)))
                             (displayCreatureLife (list-ref sortedObjects (config:player1SelectedEnemyCreature indexOfSelectedGameObjects)) defender (second (first selectedCreatureObjects)))
                                  ))
                       (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))(sortPlayer1ObjectIndex)(sortPlayer2ObjectIndex)
                       (if (>= (send defender get-attack) (send attacker get-life))
                           (begin
                             (cond
                               ((equal? (send attacker get-name) "Cultist")
                                (P1drawCard))
                               ((not (equal? (send attacker get-death-effect) "none"))
                                ((send attacker get-death-effect) 1)
                                ))
                             (removeObject (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects))
                             (set! creaturesPlayer1 (remove (list-ref creatureObjects (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects)) creaturesPlayer1))
                             (removeCreatureObject (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects))
                             (displayCreatureLife))
                           (begin
                             (send attacker set-life (- (send attacker get-life) (send defender get-attack)))
                             (displayCreatureLife (list-ref sortedObjects (config:player1SelectedFriendlyCreature indexOfSelectedGameObjects)) attacker (config:player1SelectedFriendlyCreature selectedCreatureObjects))
                             (send attacker set-sleep #t)
                                  ))
                       (refreshBoard)
                       (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                       (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                       (set! sortedObjects (sortObjects))
                       (set! sortedObjects (sortObjectsPhase2))
                       (set! creatureObjects (sortCreatureObjects)))
                      ((equal? currentTurn 2)
                       (if (>= (send attacker get-attack) (send defender get-life))
                           (begin
                             (cond
                               ((equal? (send defender get-name) "Cultist")
                                (P1drawCard))
                               ((not (equal? (send defender get-death-effect) "none"))
                                ((send defender get-death-effect) 1)
                                ))
                             (removeObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                             (set! creaturesPlayer1 (remove (list-ref creatureObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) creaturesPlayer1))
                             (removeCreatureObject (config:player2SelectedEnemyCreature indexOfSelectedGameObjects))
                             (displayCreatureLife)
                             (set-count-c! creaturesRemoved 1))
                           (begin
                             (send defender set-life (- (send defender get-life) (send attacker get-attack)))
                             (displayCreatureLife (list-ref sortedObjects (config:player2SelectedEnemyCreature indexOfSelectedGameObjects)) defender (config:player2SelectedEnemyCreature selectedCreatureObjects))
                             (set-count-c! creaturesRemoved 0)))
                       (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))(sortPlayer1ObjectIndex)(sortPlayer2ObjectIndex)
                       (if (>= (send defender get-attack) (send attacker get-life))
                           (begin
                             (cond
                               ((equal? (send attacker get-name) "Cultist")
                                (P2drawCard))
                               ((not (equal? (send attacker get-death-effect) "none"))
                                ((send attacker get-death-effect) 2)
                                ))
                             (removeObject (- (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects) (count-c creaturesRemoved)))
                             (set! creaturesPlayer2 (remove (list-ref creatureObjects (- (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects) (count-c creaturesRemoved))) creaturesPlayer2))
                             (removeCreatureObject (- (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects) (count-c creaturesRemoved)))
                             (displayCreatureLife))
                           (begin
                             (send attacker set-life (- (send attacker get-life) (send defender get-attack)))
                             (displayCreatureLife (list-ref sortedObjects (- (config:player2SelectedFriendlyCreature indexOfSelectedGameObjects) (count-c creaturesRemoved))) attacker (config:player2SelectedFriendlyCreature selectedCreatureObjects))
                             (send attacker set-sleep #t)
                             ))
                       (refreshBoard)
                       (set! indexOfSelectedGameObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                       (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))))
                   ((equal? (send attacker get-sleep) #t) #f))
                 (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))(sortPlayer1ObjectIndex)(sortPlayer2ObjectIndex)
                 (send player1LifeDisplay set-label (string-append "P1 Health: " (number->string config:P1Health)))
                 (send player2LifeDisplay set-label (string-append "P2 Health: " (number->string config:P2Health)))))

(define displayCreatureLife (λ ([gameObj '()] [creature '()] [creatureObj '()])
                              (removeCreatureLifeDisplay)
                              (cond
                                ((or (equal? gameObj '()) (equal? creature '()) (equal? creatureObj '()))
                                 (for ([i (length creatureObjects)])
                                   (cond
                                     ((not (equal? (send (list-ref creatureObjects i) get-lifeDisplay) "none"))
                                      (addObject (htdp:text (number->string (send (send (list-ref creatureObjects i) get-card) get-life)) 80 "red") (- (gameObject-x (list-ref sortedObjects i)) 68) (- (gameObject-y (list-ref sortedObjects i)) 100) 0.5 2 3))
                                     (else #f))))
                                (else
                                 (send creatureObj set-lifeDisplay (gameObject (htdp:text (number->string (send creature get-life)) 80 "red") (- (gameObject-x gameObj) 68) (- (gameObject-y gameObj) 100) 0.5 2 3))
                                 (addObject (htdp:text (number->string (send creature get-life)) 80 "red") (gameObject-x (send creatureObj get-lifeDisplay)) (gameObject-y (send creatureObj get-lifeDisplay)) (gameObject-scale (send creatureObj get-lifeDisplay)) (gameObject-layer (send creatureObj get-lifeDisplay)) 3)
                                 (send canvas on-paint)(send canvas show #t)))))
(define gameObject-of-index(λ (player index)
                             (set! sortedObjects (sortObjects))(set! sortedObjects (sortObjectsPhase2))(set! creatureObjects (sortCreatureObjects))(sortPlayer1ObjectIndex)(sortPlayer2ObjectIndex)
                             (cond
                               ((= player 1)
                                (list-ref sortedObjects (list-ref player1ObjectIndex index)))
                               ((= player 2)
                                (list-ref sortedObjects (list-ref player2ObjectIndex index))))
                                ))


                           
(provide startGUI
         addObject)