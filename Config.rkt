#lang racket

(define player1Y -100)
(define player2Y 200)

(define fiveCardHandRange  (list (list 55 295)  (list 320 555) (list 580 820) (list 840 1080) (list 1105 1345)))
(define fourCardHandRange  (list (list 185 425) (list 450 685) (list 710 950) (list 975 1210)))
(define threeCardHandRange (list (list 320 555) (list 580 820) (list 840 1080)))
(define twoCardHandRange   (list (list 450 685)'(710 950)))                
(define oneCardHandRange   (list (list 580 820)))                                                          

(provide (all-defined-out))
