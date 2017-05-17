#lang racket

(provide (all-defined-out))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

(define (silly-mode2 x)
  (letrec
      ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda(x) (if (zero? x) #f (even? (- x 1))))])
   (if (even? x) 0 1)))
