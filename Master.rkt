#lang racket/gui

(require "GUI.rkt")

(define frame (new frame% [label "Window"] [width 600] [height 400]))

(define title (read-bitmap "Image.jpg"))

(define msg (new message% [parent frame]
                          [label title]))

(new button% [parent frame]
             [label "Start Game"]
             [horiz-margin 5]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (startGUI)
                         (send frame show #f)
                         )])

(send frame show #t)
