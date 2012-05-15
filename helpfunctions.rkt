;;------------helpfunctions---------------

(define (occurs? this in-this)
      (cond ((null? in-this) #f)
            ((eq? this (car in-this)) #t)
            (else (occurs? this (cdr in-this)))))

(define (distance object1 object2)
      (sqrt (+ (sqr (- (send object1 get-x) (send object2 get-x))) (sqr (- (send object1 get-y) (send object2 get-y))))))

(define (collision? object1 object2)
  (>= (+ (send object2 get-radius) (send object1 get-radius))
      (distance object1 object2)))


(define (bunkeradder number) ;adds bunkers in the quantity of "number" to the battle field
  (let* ((sprite (make-object bitmap% "bunker.png" 'png/alpha #f))
         (radius (/ (send sprite get-width) 2))
         (generated-x (+ 300 (random 600)))
         (generated-y (+ 100 (random 300))))
    (if (= number 0) 
        (void)
        (begin (set! *object-list* (cons (new bunker%   
                                              [sprite sprite]   
                                              [radius radius]
                                              [x generated-x]
                                              [y generated-y])
                                         *object-list*))
               (bunkeradder (- number 1))))))
