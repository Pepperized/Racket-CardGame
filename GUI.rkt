#lang racket/gui

(require racket/draw
         net/url)
(require racket/draw)

(define logo
  (read-bitmap  "Image.jpg"))


(define frame (new frame%
                   [label "Window"] [width 1400] [height 800]
                   [style '(no-resize-border)]
                   ))

(define canvas (new canvas% [parent frame]))

(void (new message% [parent frame] [label logo]))

 
; Make a static text message in the frame
(define msg (new message% [parent canvas]
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