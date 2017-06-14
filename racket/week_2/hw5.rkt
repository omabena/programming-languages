;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; (a)
(define (racketlist->mupllist l)
  (cond [(null? l) (aunit)]
        [#t (apair (car l) (racketlist->mupllist (cdr l)))]))
  
;; (b)
(define (mupllist->racketlist mupll)
  (cond [(apair? mupll) (cons (apair-e1 mupll) (mupllist->racketlist (apair-e2 mupll)))]
        [#t null]))
 

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))



;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error (format "MUPL addition applied to non-number: ~v" v2))))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (ifgreater-e3 e)
                   (ifgreater-e4 e))
               (error "compare has to be int")))]
        [(mlet? e)
         (let ([envmlet (list (cons (mlet-var e) (eval-under-env (mlet-e e) env)))])
           (eval-under-env (mlet-body e) envmlet))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (letrec ([cf (closure-fun v1)]
                      [cenv (closure-env v1)]
                      [funname (cons (fun-nameopt cf) v1)]
                      [funvar (cons (fun-formal cf) v2)]) 
                 (eval-under-env (fun-body cf)
                                 (if (eq? (car funname) #f)
                                     (cons funvar cenv)
                                     (cons funvar (cons funname cenv)))))
               (error "should return a closure")))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(fst? e)
         (let ([apair-e (eval-under-env (fst-e e) env)])
           (if (apair? apair-e)
               (apair-e1 apair-e)
               (error "this is not a vailid apair")))]
        [(snd? e)
         (let ([apair-e (eval-under-env (snd-e e) e)])
           (if (apair? apair-e)
               (apair-e2 apair-e)
               (error "this is not a vailid apair")))]
        [(apair? e)
         (let ([first (eval-under-env (apair-e1 e) env)]
               [second (eval-under-env (apair-e2 e) env)])
           (apair first second))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([ans (eval-under-env (isaunit-e e) env)])
           (if (aunit? ans) (int 1) (int 0)))]
         [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3)) 

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
