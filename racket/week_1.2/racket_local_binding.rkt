#lang racket

(provide (all-defined-out))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))


(define (silly-double_2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

(define (silly-double_3 x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)])
    (f - 9)))

(define (silly-mod2 x)
  (letrec
      ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda(x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))
       
       
           