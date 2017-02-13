#lang racket

(define currentPlayerY -100)
(define otherPlayerY 200)

(define fiveCardHandRange  (list (list 55 295)  (list 320 555) (list 580 820) (list 840 1080) (list 1105 1345)))
(define fourCardHandRange  (list (list 185 425) (list 450 685) (list 710 950) (list 975 1210)))
(define threeCardHandRange (list (list 320 555) (list 580 820) (list 840 1080)))
(define twoCardHandRange   (list (list 450 685) (list 710 950)))                
(define oneCardHandRange   (list (list 580 820)))                                                          

(define oneCreatureBoardRange   (list (list 615 785 )))
(define twoCreatureBoardRange   (list (list 515 685) (list 715 885)))
(define threeCreatureBoardRange (list (list 415 585) (list 615 785) (list 815 985)))
(define fourCreatureBoardRange  (list (list 315 485) (list 515 685) (list 715 885) (list 915 1085)))
(define fiveCreatureBoardRange  (list (list 215 385) (list 415 585) (list 615 785) (list 815 985) (list 1015 1185)))

(provide (all-defined-out))
