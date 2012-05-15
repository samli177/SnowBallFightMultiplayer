;---superclass for object that can be displayed on screen as sprite---
(define on-screen% 
  
  (class object%
    (super-new)
    
    (init-field (x 0) (y 0)   ;; position of object on screen 
                (radius 0)   ;; space occupied by object on screen
                (sprite "")) ;; path to image representing object on screen 
    
    
    ;---------------set-methods-------------------
    (define/public (set-xy! new-x new-y) (begin (set! x new-x) (set! y new-y)))
    (define/public (set-x! new-x) (set! x new-x))
    (define/public (set-y! new-y) (set! y new-y))
    (define/public (set-radius! new-radius) (set! radius new-radius))
    (define/public (set-sprite! new-sprite) (set! sprite new-sprite))
    
    
    ;--------------get-methods-------------------
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-radius) radius)
    (define/public (get-sprite) sprite)
    
    
    ;--------------actions--------------------
    
    (define/public (move) #f)))
    
(define powerbar%
  (class on-screen%
    (inherit set-sprite!)
    (super-new)
    (init-field (power 0))
    
    ;--------------objects-images-------------
    
    (define kraft0 (make-object bitmap% "kraft0.png" 'png/alpha #f))
    (define kraft1 (make-object bitmap% "kraft1.png" 'png/alpha #f))
    (define kraft2 (make-object bitmap% "kraft2.png" 'png/alpha #f))
    (define kraft3 (make-object bitmap% "kraft3.png" 'png/alpha #f))
    (define kraft4 (make-object bitmap% "kraft4.png" 'png/alpha #f))
    (define kraft5 (make-object bitmap% "kraft5.png" 'png/alpha #f))
    
    
    ;--------------set-methods----------------
    (define/public (set-power! new-power) (begin (set! power new-power) 
                                                 (case power
                                                   ((= power 0) (set-sprite! kraft0))
                                                   ((= power 5) (set-sprite! kraft1))
                                                   ((= power 10) (set-sprite! kraft2))
                                                   ((= power 15) (set-sprite! kraft3))
                                                   ((= power 20) (set-sprite! kraft4))
                                                   ((>= power 25) (set-sprite! kraft5)))))
                                                   
    ;--------------get-methods----------------
    (define/public (get-power) power)))
 
    


(define player%
  (class on-screen%
    (inherit get-x get-y get-radius)
    (super-new)
    (init-field (hp 2)   ; hitpoints
                (side 1) ; positive if player is on the left
                (speed 30)
                (power 0)
                (powerbar #f)
                (youlosepic (make-object bitmap% "youlosepic.png" 'png/alpha #f)))
  
    ;---------------set-methods-------------------
    (define/public (set-hp! new-hp) (set! hp new-hp))
    (define/public (set-side! new-side) (set! side new-side))
    (define/public (set-speed! new-speed) (set! speed new-speed))
    (define/public (set-power! new-power) (set! power new-power))
  
    ;---------------get-methods------------------
    (define/public (get-hp) hp)
    (define/public (alive?) (not (= hp 0)))
    (define/public (get-side) side)
    (define/public (get-speed) speed)
    (define/public (get-power) power)
    (define/public (get-powerbar) powerbar)
  
    ;---------------actions----------------------
    (define/public (power-up!) (set! power (+ 1 power)))
    (define/public (power-down!) (set! power 0))
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) (begin  (set! *object-list* (cons (new on-screen% 
                                                                                                 [sprite youlosepic]
                                                                                                 [x 400]
                                                                                                 [y 300]) *object-list*))
                                                                  (sleep 0.1)
                                                                  (send new-game pause-update))))
    (define/public (throw) (let((old-power power))
                             (begin
                               (set! power 0)
                               (new snowball% 
                                    [sprite (make-object bitmap% "snowballe.png" 'png/alpha #f)]
                                    [x (+ (* side (+ (get-radius) 2)) (get-x))]
                                    [y (get-y)]
                                    [speed (* side old-power)]))))
    
    (define/public (update-powerbar!) (begin (if (not (eq? power (send powerbar get-power))) (send powerbar set-power! power))
                                             (send powerbar set-x! (get-x))
                                             (send powerbar set-y! (- (get-y) (get-radius)))))))

                                
    
    

  


(define snowball%
  (class on-screen%
    (super-new)
    (inherit set-x! get-x)
    (init-field (speed 10) (duration 100) (counter 0)) ; speed is negative for snowballs thrown left
    
    ;---------------set-methods-----------------
    (define/public (set-throw_param! new-speed new-duration) (begin (set! speed new-speed) 
                                                                    (set! duration new-duration)))
    
    (define/public (set-power! power) (begin (set! speed power) 
                                             (set! duration (abs power)))) ; likely to be modifyed
    
    ;---------------get-methods------------------
    (define/public (get-speed) speed)
    (define/public (get-duration) duration)
    
    ;---------------actions---------------------
    (define/override (move) ; Returns #t when maximum duration is reached
      (begin 
        (if (and (> speed 0)(= 10 counter)) (begin (set! speed (- speed 1)) (set! counter 0)))
        (if (and (< speed 0)(= 10 counter)) (begin (set! speed (+ speed 1)) (set! counter 0)))
        (set! counter (+ counter 1))
        (set-x! (+ (get-x) speed)) 
        (set! duration (- duration 1)) (= duration 0)))))


(define bunker%
  (class on-screen%
    ;(inherit get-x get-y get-radius)
    (super-new)
    (field (hp 100)) ; hitpoints
  
    ;---------------set-methods-------------------
    (define/public (set-hp! new-hp) (set! hp new-hp))
  
    ;---------------get-methods------------------
    (define/public (get-hp) hp)
    (define/public (broken?) (not (= hp 0)))
    ;---------------actions----------------------
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) (stop-update)))))
  
    
;---------testing--------------
(define *test-osc* (new on-screen%))
(send *test-osc* set-xy! 1 2)

(define *test-pla* (new player%))

(define *test-snow* (new snowball%))

(define *test-bunker* (new bunker%))

;----------instances-------------


(define *player*
  (new player% 
       [sprite (make-object bitmap% "blue_player.png" 'png/alpha #f)]
       [powerbar (new powerbar% [sprite (make-object bitmap% "kraft0.png" 'png/alpha #f)])]))

(send *player* set-radius! (round (/ (send (send *player* get-sprite) get-height) 2)))

                
     


(define *network* (new network-session%))


;------------pictures-----------



(define *object-list* (list *player* (send *player* get-powerbar)))





