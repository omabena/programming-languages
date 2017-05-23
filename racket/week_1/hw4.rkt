
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; 1.
(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))])) 

; 2.               
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))
  
; 3.
(define (list-nth-mod xs n)
  (cond [(< n 0) error "list-nth-mod: negative number"]
        [(null? xs) error "list-nth-mod: empty list"]
        [#t (let
                ([ith (remainder n (length xs))])
              (car (list-tail xs ith)))]))

(define ones (lambda() (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 4.
(define (stream-for-n-steps s n)
  (letrec ([f (lambda(cur n l)
                (cond [(= n 0) l]
                      [#t (f (cdr (cur)) (- n 1) (append l (cons (car (cur)) null)))]))])
    (f s n null)))

; 5.
(define funny-number-stream
  (letrec ([f (lambda(x)
                (cons (cond [(= 0 (modulo x 5)) (* -1 x)]
                            [#t x])
                      (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

; 6.
(define dan-then-dog
  (letrec ([f (lambda(s)
                (cons s (lambda() (f (cond [(string=? s "dan.jpg") "dog.jpg"]
                                                   [#t "dan.jpg"])))))])
    (lambda() (f "dan.jpg"))))

; 7.
(define (stream-add-zero stream)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda() (f (cdr (s))))))])
    (lambda() (f stream))))

; 8.
(define (cycle-lists xs ys)
  (letrec ([stream (lambda(cur_xs cur_ys)
                     (cons (cons (car cur_xs) (car cur_ys))
                           (lambda () (stream (cond [(null? (cdr cur_xs)) xs]
                                                    [#t (cdr cur_xs)])
                                              (cond [(null? (cdr cur_ys)) ys]
                                                    [#t (cdr cur_ys)])))))])
    (lambda() (stream xs ys))))
  
  
; 9.
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [helper (lambda(cur) (cond [(> (+ 1 cur) l) #f]
                                      [(pair? (vector-ref vec cur))
                                       (let ([pr (vector-ref vec cur)])
                                         (cond [(equal? v (car pr)) pr]
                                               [#t (helper (+ 1 cur))]))]
                                      [#t (helper (+ 1 cur))]))])
    (helper 0)))

; 10.

(define (cached-assoc xs n)
  (letrec ([cache (make-vector 20)]
           [cache_pos 1]
           [f (lambda(x)
                (if (vector-assoc n cache)
                    (begin
                      (vector-assoc n cache))
                    (begin
                      (vector-set! cache cache_pos n)
                      (set! cache_pos (+ 1 cache_pos))
                      (assoc n xs))))])
    f))
              
              
              
              
        
    
  

  
                  