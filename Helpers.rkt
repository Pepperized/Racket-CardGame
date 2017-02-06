#lang racket

(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define (make-unique-filter)
  (let ((seen '()))
    (lambda (e)
      (if (member e seen)
          #f
          (begin
            (set! seen (cons e seen))
            #t)))))

(define (removed2 lst)
  (filter (make-unique-filter) lst))

(provide index-of
         make-unique-filter
         removed2)