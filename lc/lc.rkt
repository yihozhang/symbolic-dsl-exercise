#lang rosette/safe
(require rosette/lib/match)
(require rosette/lib/synthax)
(require rosette/lib/angelic)

(struct v (x) #:transparent)
(struct app (x y) #:transparent)
(struct abs (x T y) #:transparent)
(struct zero () #:transparent)
(struct succ (x) #:transparent)

(define (interpret program store)
  (match program
    [(v x) (let ([x (assoc x store)])
             (if x (cadr x) 'stuck))]
    [(app x y) (let ([x (interpret x store)]
                     [y (interpret y store)])
                 (match x
                   [(abs n T p) (match y
                                  ['stuck 'stuck]
                                  [_ (interpret p (cons (list n y) store))])]
                   [_ 'stuck]))]
    [(abs x T y) (abs x T y)]
    [(zero) 0]
    [(succ x) (let [(x (interpret x store))]
                (if (number? x)
                    (+ x 1)
                    'stuck))]
    [_ 'stuck]))

(define (valid-syntax? program store)
  (match program
    [(v x) (if (member x store) #t #f)]
    [(app x y) (and (valid-syntax? x store)
                    (valid-syntax? y store))]
    [(abs x T y) (valid-syntax? y (cons x store))]
    [(zero) #t]
    [(succ x) (valid-syntax? x store)]
    [_ #f]))
(struct Num () #:transparent)
(struct Function (a b) #:transparent)

(define (type-of program type-store)
  (match program
    [(v x) (let ([x (assoc x type-store)])
             (if x (cadr x) 'ill-typed))]
    [(app x y) (let ([tx (type-of x type-store)]
                     [ty (type-of y type-store)])
                 (match tx
                   [(Function a b) (if (eq? a ty)
                                       b
                                       'ill-typed)]
                   [_ 'ill-typed]))]
    [(abs x T y) (Function T
                           (type-of y (cons (list x T) type-store)))]
    [(zero) (Num)]
    [(succ x) (if (eq? (type-of x type-store) (Num))
                  (Num)
                  'ill-typed)]
    [_ 'ill-typed]))
                 

(define program-1
  (app (abs 'x (Num) (v 'x)) (zero)))
(define program-2
  (abs  'x (Num) (succ (v 'x))))
(define bad-program-1
  (app (abs 'x (Num) (v 'y)) (succ (succ (zero)))))

(define bad-program-2
  (app (zero) (zero)))

(define-synthax (ST d)
  #:base (Num)
  #:else (choose*
          (Function (ST (- d 1)) (ST (- d 1)))
          (Num)))

(define-synthax (STLC [c1 ...] d)
  #:base (choose* (v (choose c1 ...))
                  (zero))
  #:else (choose*
          (v (choose* c1 ...))
          (app (STLC [c1 ...] (- d 1)) (STLC [c1 ...] (- d 1)))
          (abs (STLC [c1 ...] (- d 1)) (ST (- d 1)) (STLC [c1 ...] (- d 1)))
          (zero)
          (succ (STLC [c1 ...] (- d 1)))))


;(assert (= (interpret program-1 '()) 0))
;(assert (eq? (interpret program-2 '()) program-2))
;(assert (valid-syntax? program-1 '()))
;(assert (not (valid-syntax? bad-program-1 '())))
;(assert (valid-syntax? bad-program-2 '()))
;(assert (eq? (type-of program-1 '()) (Num)))
;(assert (eq? (type-of program-2 '()) (Function (Num) (Num))))
;(define model (synthesize #:forall '()
;            #:guarantee (assert (let ([program (STLC ['a] 2)])
;                          (valid-syntax? program '())))))
(define program (STLC ['a] 1))
(define model (solve (assert (and (valid-syntax? program '())
                              (eq? 'stuck (interpret program '()))))))