#lang racket/gui

(require racket/draw
         net/url
         mred)

(require (prefix-in htdp: 2htdp/image))

(define logo
  (read-bitmap  "Image.jpg"))


(define frame (new frame%
                   [label "Window"] [width 1400] [height 800]
                   [style '(no-resize-border)]
                   ))



(void (new message% [parent frame] [label logo]))


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


(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (init-field [bitmapX 0])
    (init-field [bitmapY 0])
    (init-field [bitmap-scale 1])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap
            (scale-bitmap bitmap bitmap-scale)
            bitmapX bitmapY
            ))
    (super-new)))



(define canvas (new bitmap-canvas% [parent frame] [bitmap logo] [bitmapX 100] [bitmap-scale 0.5]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define bottomFrame (new horizontal-panel% [parent frame] [alignment '(right bottom)]))

; Make a button in the frame
(new button% [parent bottomFrame]
             [label "Show Hand"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(new button% [parent bottomFrame]
             [label "End Turn"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Here's your hand"))])

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

(provide startGUI)

; Show the frame by calling its show method
;(send frame show #t)