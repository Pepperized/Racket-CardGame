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

(define objects '())
(define sortedObjects '())
(define player1ObjectIndex '())
(define player2ObjectIndex '())
(define creaturesPlayer1 '())
(define creaturesPlayer2 '())
(define creatureObjects '())                                                                                                                 ;creature(Objects) selected by P1               ;creatureObjects selected by P2

(define selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))) ; format: '( '(P1CreatureObjectIndex P2CreatureObjectIndex) '(P1CreatureObjectIndex P2CreatureObjectIndex) )
(define selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))       ; format: '( '(P1selectedCreature P2selectedCreature) '(P1selectedCreature P2SelectedCreature) )

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
        ((send event button-down? 'left) (println (string-append "X: " (number->string (send event get-x)) ", Y: " (number->string (send event get-y)))) (selectBoardCreature event))
        ((send event button-down? 'right) (begin
                                         (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                         (set! selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                                         (cond
                                           ((not (equal? (spellStruct-name activeSpell) "none")) (cancelSpell) (send hand-canvas on-paint) ))))))
                                      
    (define/public selectBoardCreature (λ (event)
                              (let ([x (send event get-x)])
                                (let ([y (send event get-y)])
                                  (cond
                                    ((equal? currentTurn 1) 
                                     (cond
                                       ((and (> y (first config:player1Y-BoardRange)) (< y (second config:player1Y-BoardRange)))
                                        (cond
                                          ((equal? (spellStruct-name activeSpell) "none")
                                           (cond
                                             ((equal? (length creaturesPlayer1) 1)
                                              (cond
                                                ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                 (set! selectedCreatureObjects (append (list (cons (first player1ObjectIndex) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                 (set! selectedCreatures (append (list (cons (first creaturesPlayer1) (rest (first selectedCreatures)))) (rest selectedCreatures))))))
                                             ((equal? (length creaturesPlayer1) 2)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))))))
                                             ((equal? (length creaturesPlayer1) 3)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))))))
                                             ((equal? (length creaturesPlayer1) 4)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))))))
                                             ((equal? (length creaturesPlayer1) 5)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))))))))
                                          ((not (equal? activeSpell "none"))
                                           ((equal? (length creaturesPlayer1) 1)
                                              (cond
                                                ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                 (set! selectedCreatureObjects (append (list (cons (first player1ObjectIndex) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                 (set! selectedCreatures (append (list (cons (first creaturesPlayer1) (rest (first selectedCreatures)))) (rest selectedCreatures)))
                                                 ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (first selectedCreatures)) get-card)))))
                                             ((equal? (length creaturesPlayer1) 2)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (first selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 3)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (first selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 4)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (first selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 5)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (cons (list-ref player1ObjectIndex i) (rest (first selectedCreatureObjects)))) (rest selectedCreatureObjects)))
                                                   (set! selectedCreatures (append (list (cons (list-ref creaturesPlayer1 i) (rest (first selectedCreatures)))) (rest selectedCreatures)))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (first selectedCreatures)) get-card)))))))))
                                          ((and (> y (first config:player2Y-BoardRange)) (< y (second config:player2Y-BoardRange)))
                                           (cond
                                             ((equal? (spellStruct-name activeSpell) "none")
                                              (cond
                                                ((equal? (length creaturesPlayer2) 1)
                                                 (cond
                                                   ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                    (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (first player2ObjectIndex)))) (rest selectedCreatureObjects)))
                                                    (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (first creaturesPlayer2)))) (rest selectedCreatures)))
                                                    (cond
                                                      ((not (equal? (first (first selectedCreatures)) config:noCreature))
                                                       (attack (send (first (first selectedCreatures)) get-card) (send (second (first selectedCreatures)) get-card)))))))
                                                ((equal? (length creaturesPlayer2) 2)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                      (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                      (cond
                                                        ((not (equal? (first (first selectedCreatures)) config:noCreature))
                                                         (attack (send (first (first selectedCreatures)) get-card) (send (second (first selectedCreatures)) get-card))))))))
                                                ((equal? (length creaturesPlayer2) 3)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                      (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                      (cond
                                                        ((not (equal? (first (first selectedCreatures)) config:noCreature))
                                                         (attack (send (first (first selectedCreatures)) get-card) (send (second (first selectedCreatures)) get-card))))))))
                                                ((equal? (length creaturesPlayer2) 4)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                      (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                      (cond
                                                        ((not (equal? (first (first selectedCreatures)) config:noCreature))
                                                         (attack (send (first (first selectedCreatures)) get-card) (send (second (first selectedCreatures)) get-card))))))))
                                                ((equal? (length creaturesPlayer2) 5)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                      (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                      (cond
                                                        ((not (equal? (first (first selectedCreatures)) config:noCreature))
                                                         (attack (send (first (first selectedCreatures)) get-card) (send (second (first selectedCreatures)) get-card))))))))))
                                              ((not (equal? (spellStruct-name activeSpell) "none"))
                                               (cond
                                                 ((equal? (length creaturesPlayer2) 1)
                                                  (cond
                                                    ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                     (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (first player2ObjectIndex)))) (rest selectedCreatureObjects)))
                                                     (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (first creaturesPlayer2)))) (rest selectedCreatures)))
                                                     ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (first selectedCreatures)) get-card)))))
                                                 ((equal? (length creaturesPlayer2) 2)
                                                  (for ([i (length creaturesPlayer2)])
                                                    (cond
                                                      ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                       (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                       (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                       ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (first selectedCreatures)) get-card))))))
                                                 ((equal? (length creaturesPlayer2) 3)
                                                  (for ([i (length creaturesPlayer2)])
                                                    (cond
                                                      ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                       (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                       (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                       ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (first selectedCreatures)) get-card))))))
                                                 ((equal? (length creaturesPlayer2) 4)
                                                  (for ([i (length creaturesPlayer2)])
                                                    (cond
                                                      ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                       (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                       (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                       ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (first selectedCreatures)) get-card))))))
                                                 ((equal? (length creaturesPlayer2) 5)
                                                  (for ([i (length creaturesPlayer2)])
                                                    (cond
                                                      ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                       (set! selectedCreatureObjects (append (list (cons (first (first selectedCreatureObjects)) (list (list-ref player2ObjectIndex i)))) (rest selectedCreatureObjects)))
                                                       (set! selectedCreatures (append (list (cons (first (first selectedCreatures)) (list (list-ref creaturesPlayer2 i)))) (rest selectedCreatures)))
                                                       ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (first selectedCreatures)) get-card))))))))))))
                                    ((equal? currentTurn 2) 
                                     (cond
                                       ((and (> y (first config:player1Y-BoardRange)) (< y (second config:player1Y-BoardRange)))
                                        (cond
                                          ((equal? (spellStruct-name activeSpell) "none")
                                           (cond
                                             ((equal? (length creaturesPlayer1) 1)
                                              (cond
                                                ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                 (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first player1ObjectIndex) (list (second (first (rest selectedCreatureObjects))))))))
                                                 (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first creaturesPlayer1) (list (second (first (rest selectedCreatures))))))))
                                                 (cond
                                                   ((not (equal? (second (second selectedCreatures)) config:noCreature))
                                                    (attack (send (second (second selectedCreatures)) get-card) (send (first (second selectedCreatures)) get-card)))))))
                                             ((equal? (length creaturesPlayer1) 2)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   (cond
                                                     ((not (equal? (second (second selectedCreatures)) config:noCreature))
                                                      (attack (send (second (second selectedCreatures)) get-card) (send (first (second selectedCreatures)) get-card))))))))
                                             ((equal? (length creaturesPlayer1) 3)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   (cond
                                                     ((not (equal? (second (second selectedCreatures)) config:noCreature))
                                                      (attack (send (second (second selectedCreatures)) get-card) (send (first (second selectedCreatures)) get-card))))))))
                                             ((equal? (length creaturesPlayer1) 4)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   (cond
                                                     ((not (equal? (second (second selectedCreatures)) config:noCreature))
                                                      (attack (send (second (second selectedCreatures)) get-card) (send (first (second selectedCreatures)) get-card))))))))
                                             ((equal? (length creaturesPlayer1) 5)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   (cond
                                                     ((not (equal? (second (second selectedCreatures)) config:noCreature))
                                                      (attack (send (second (second selectedCreatures)) get-card) (send (first (second selectedCreatures)) get-card))))))))))
                                          ((not (equal? (spellStruct-name activeSpell) "none"))
                                           (cond
                                             ((equal? (length creaturesPlayer1) 1)
                                              (cond
                                                ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                 (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first player1ObjectIndex) (list (second (first (rest selectedCreatureObjects))))))))
                                                 (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first creaturesPlayer1) (list (second (first (rest selectedCreatures))))))))
                                                 ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (second selectedCreatures)) get-card)))))
                                             ((equal? (length creaturesPlayer1) 2)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (second selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 3)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (second selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 4)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (second selectedCreatures)) get-card))))))
                                             ((equal? (length creaturesPlayer1) 5)
                                              (for ([i (length creaturesPlayer1)])
                                                (cond
                                                  ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                   (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (list-ref player1ObjectIndex i) (list (second (first (rest selectedCreatureObjects))))))))
                                                   (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (list-ref creaturesPlayer1 i) (list (second (first (rest selectedCreatures))))))))
                                                   ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (first (second selectedCreatures)) get-card))))))))
                                          ((and (> y (first config:player2Y-BoardRange)) (< y (second config:player2Y-BoardRange)))
                                           (cond
                                             ((equal? (spellStruct-name activeSpell) "none")
                                              (cond
                                                ((equal? (length creaturesPlayer2) 1)
                                                 (cond
                                                   ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                    (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (first player2ObjectIndex))))))
                                                    (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (first creaturesPlayer2)))))))))
                                                ((equal? (length creaturesPlayer2) 2)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))))))
                                                ((equal? (length creaturesPlayer2) 3)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))))))
                                                ((equal? (length creaturesPlayer2) 4)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))))))
                                                ((equal? (length creaturesPlayer2) 5)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))))))))
                                             ((not (equal? (spellStruct-name activeSpell) "none"))
                                              (cond
                                                ((equal? (length creaturesPlayer2) 1)
                                                 (cond
                                                   ((and (> x (first (first config:oneCreatureBoardRange))) (< x (second (first config:oneCreatureBoardRange))))
                                                    (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (first player2ObjectIndex))))))
                                                    (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (first creaturesPlayer2))))))
                                                    ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (second selectedCreatures)) get-card)))))
                                                ((equal? (length creaturesPlayer2) 2)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:twoCreatureBoardRange i))) (< x (second (list-ref config:twoCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))
                                                      ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (second selectedCreatures)) get-card))))))
                                                ((equal? (length creaturesPlayer2) 3)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:threeCreatureBoardRange i))) (< x (second (list-ref config:threeCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))
                                                      ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (second selectedCreatures)) get-card))))))
                                                ((equal? (length creaturesPlayer2) 4)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fourCreatureBoardRange i))) (< x (second (list-ref config:fourCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))
                                                      ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (second selectedCreatures)) get-card))))))
                                                ((equal? (length creaturesPlayer2) 5)
                                                 (for ([i (length creaturesPlayer2)])
                                                   (cond
                                                     ((and (> x (first (list-ref config:fiveCreatureBoardRange i))) (< x (second (list-ref config:fiveCreatureBoardRange i))))
                                                      (set! selectedCreatureObjects (append (list (first selectedCreatureObjects)) (list (cons (first (first (rest selectedCreatureObjects))) (list (list-ref player2ObjectIndex i))))))
                                                      (set! selectedCreatures (append (list (first selectedCreatures)) (list (cons (first (first (rest selectedCreatures))) (list (list-ref creaturesPlayer2 i))))))
                                                      ((send (getCard (spellStruct-name activeSpell)) get-effect) (send (second (second selectedCreatures)) get-card)))))))))))))))))))
                                  
                                  
                                        
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
                                       (send (send (list-ref creaturesPlayer1 i) get-card) set-sleep #f))))
                                  ((equal? currentTurn 2)
                                   (if (not (equal? (spellStruct-name activeSpell) "none")) (cancelSpell) #f)
                                   (for ([i (length creaturesPlayer2)])
                                     (when (equal? (send (send (list-ref creaturesPlayer2 i) get-card) get-sleep) #t)
                                       (send (send (list-ref creaturesPlayer2 i) get-card) set-sleep #f)))))
                                (endTurn)))])

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
                                 (for ([i (length sortedObjects)])
                                   (when (equal? (gameObject-y (list-ref sortedObjects i)) config:player1Y)
                                     (set! player1ObjectIndex (append player1ObjectIndex (list i)))))
                                 (set! player1ObjectIndex (removed2 player1ObjectIndex))))
                                    
(define sortPlayer2ObjectIndex (λ ()
                                 (set! player2ObjectIndex '())
                                 (for ([i (length sortedObjects)])
                                       (when (equal? (gameObject-y (list-ref sortedObjects i)) config:player2Y)
                                         (set! player2ObjectIndex (append player2ObjectIndex (list i)))))
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
                          (send canvas show #t)))))
(define playCard (λ (pos)
                   (cond
                     ((equal? currentTurn 1)
                      (let ([cardMana (send (list-ref P1hand pos) get-mana)])
                        (cond
                          ((>= (mana-currentMana P1Mana) cardMana)
                           (cond
                             ((is-a? (list-ref P1hand pos) spell%)
                              (set-spellStruct-name! activeSpell (send (list-ref P1hand pos) get-name))
                              (removeCardFromHand pos)
                              (send hand-canvas on-paint)
                              (send hand-canvas show #t))
                             ((is-a? (list-ref P1hand pos) creature%)
                              (let ([x (packageCardObject (list-ref P1hand pos) pos)])
                                (set! creatureObjects (append creatureObjects (list x)))
                                (set! creaturesPlayer1 (append creaturesPlayer1 (list x)))
                                (send x set-index (index-of creaturesPlayer1 x))
                                (send (send x get-card) set-sleep #t)
                                (addObject (send (send x get-card) get-image) 0 config:player1Y 0.5 1)
                                (removeCardFromHand pos)
                                (set-mana-currentMana! P1Mana (- (mana-currentMana P1Mana) cardMana))
                                (send hand-canvas on-paint)
                                (send hand-canvas show #t)
                                (refreshBoard)))))
                          ((< (mana-currentMana P1Mana) cardMana) #f))))
                      ((equal? currentTurn 2)
                       (let ([cardMana (send (list-ref P2hand pos) get-mana)])
                        (cond
                          ((>= (mana-currentMana P2Mana) cardMana)
                           (cond
                             ((is-a? (list-ref P2hand pos) spell%))
                             ((is-a? (list-ref P2hand pos) creature%)
                              (let ([x (packageCardObject (list-ref P2hand pos) pos)])
                                (set! creatureObjects (append creatureObjects (list x)))
                                (set! creaturesPlayer2 (append creaturesPlayer2 (list x)))
                                (send x set-index (index-of creaturesPlayer2 x))
                                (addObject (send (send x get-card) get-image) 0 config:player2Y 0.5 1)
                                (removeCardFromHand pos)
                                (set-mana-currentMana! P2Mana (- (mana-currentMana P2Mana) cardMana))
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
                       (cond
                         ((is-a? defender creature%)
                          (cond
                            ((>= (send attacker get-attack) (send defender get-life))
                             (removeObject (second (first selectedCreatureObjects)))
                             (set! creaturesPlayer2 (remove (list-ref creatureObjects (second (first selectedCreatureObjects))) creaturesPlayer2))
                             (removeCreatureObject (second (first selectedCreatureObjects)))
                             (refreshBoard)
                             (send attacker set-sleep #t)
                             (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                             (set! selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))
                            ((< (send attacker get-attack) (send defender get-life))
                             (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                             (set! selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))))
                         (#t #f)))
                      ((equal? currentTurn 2)
                       (cond
                         ((is-a? defender creature%)
                          (cond
                            ((>= (send attacker get-attack) (send defender get-life))
                             (removeObject (first (second selectedCreatureObjects)))
                             (set! creaturesPlayer1 (remove (list-ref creatureObjects (first (second selectedCreatureObjects))) creaturesPlayer1))
                             (removeCreatureObject (first (second selectedCreatureObjects)))
                             (refreshBoard)
                             (send attacker set-sleep #t)
                             (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                             (set! selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))
                            ((< (send attacker get-attack) (send defender get-life))
                             (set! selectedCreatureObjects (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature)))
                             (set! selectedCreatures (list (list config:noCreature config:noCreature) (list config:noCreature config:noCreature))))))
                         (#t #f)))))
                   ((equal? (send attacker get-sleep) #t) #f))))

(provide startGUI
         addObject
         )