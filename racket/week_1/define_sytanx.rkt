#lang racket

(provide (all-defined-out))

(define-syntax my-if
  (syntax-rules (then else)
    ([my-if e1 then e2 else e3]
     (if e1 e2 e3))))

(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

#|(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda() e))]))
|#
;(f (my-delay e))