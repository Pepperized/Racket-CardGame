#lang racket/gui

(require racket/draw
         net/url)



(define frame (new frame% [label "Window"] [width 1400] [height 800]))

(define canvas (new canvas% [parent frame]))

(void (new message% [parent frame] [label logo]))

 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(define menu-bar (new menu-bar%
                      (parent frame)))

(define file (new menu%
     (label "&File")
     (parent menu-bar)))

(define choice1 (new menu-item%
                     [parent file] [label "New Game"] [callback (lambda (button event)
                         (send msg set-label "Button click"))]))
 
; Show the frame by calling its show method
(send frame show #t)