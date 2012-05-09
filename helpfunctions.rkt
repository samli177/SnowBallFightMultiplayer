;;------------helpfunctions---------------

(define (occurs? this in-this)
      (cond ((null? in-this) #f)
            ((eq? this (car in-this)) #t)
            (else (occurs? this (cdr in-this)))))





(define (distance object1 object2)
      (sqrt (+ (sqr (- (send object1 get-x) (send object2 get-x))) (sqr (- (send object1 get-y) (send object2 get-y))))))