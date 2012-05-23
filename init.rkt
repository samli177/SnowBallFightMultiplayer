;---superclass for object that can be displayed on screen as sprite---
(define on-screen% 
  
  (class object%
    (super-new)
    
    (init-field (x 0) (y 0)   ;; position of object on screen 
                (radius 0)   ;; space occupied by object on screen
                (sprite "")
                (position-semaphore (make-semaphore 1))) ;; path to image representing object on screen 
    
    
    ;---------------set-methods-------------------
    (define/public (set-xy! new-x new-y) 
      (begin 
        (semaphore-wait position-semaphore)
        (set! x new-x) 
        (set! y new-y)
        (semaphore-post position-semaphore)))
    
    (define/public (set-x! new-x) 
      (semaphore-wait position-semaphore)
      (set! x new-x)
      (semaphore-post position-semaphore))
    
    (define/public (set-y! new-y) 
      (semaphore-wait position-semaphore)
      (set! y new-y)
      (semaphore-post position-semaphore))
    
    (define/public (set-radius! new-radius) (set! radius new-radius))
    (define/public (set-sprite! new-sprite) (set! sprite new-sprite))
    
    
    ;--------------get-methods-------------------
    (define/public (get-x) 
      (let ((temp 0))
        (begin
          (semaphore-wait position-semaphore)
          (set! temp x)
          (semaphore-post position-semaphore)
          temp)))
    
    (define/public (get-y) 
      (let ((temp 0))
        (begin
          (semaphore-wait position-semaphore)
          (set! temp y)
          (semaphore-post position-semaphore)
          temp)))
    
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
    
    (define kraft0 (make-object bitmap% "pics/kraft0.png" 'png/alpha #f))
    (define kraft1 (make-object bitmap% "pics/kraft1.png" 'png/alpha #f))
    (define kraft2 (make-object bitmap% "pics/kraft2.png" 'png/alpha #f))
    (define kraft3 (make-object bitmap% "pics/kraft3.png" 'png/alpha #f))
    (define kraft4 (make-object bitmap% "pics/kraft4.png" 'png/alpha #f))
    (define kraft5 (make-object bitmap% "pics/kraft5.png" 'png/alpha #f))
    
    
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
                (weapon #f)
                (snowball-sprite (make-object bitmap% "pics/snowballe.png" 'png/alpha #f))
                (weapon-sprite (make-object bitmap% "pics/weapon-projectile.png" 'png/alpha #f))
                (youlosepic (make-object bitmap% "pics/youlosepic.png" 'png/alpha #f)))
    
    ;---------------set-methods-------------------
    (define/public (set-hp! new-hp) (set! hp new-hp))
    (define/public (set-side! new-side) (set! side new-side))
    (define/public (set-speed! new-speed) (set! speed new-speed))
    (define/public (set-power! new-power) (set! power new-power))
    (define/public (set-weapon! new-weapon) (set! weapon new-weapon))
    
    ;---------------get-methods------------------
    (define/public (get-hp) hp)
    (define/public (alive?) (not (= hp 0)))
    (define/public (get-side) side)
    (define/public (get-speed) speed)
    (define/public (get-power) power)
    (define/public (get-powerbar) powerbar)
    (define/public (get-weapon) weapon)
    
    ;---------------actions----------------------
    (define/public (power-up!) (set! power (+ 1 power)))
    (define/public (power-down!) (set! power 0))
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) 
                              (begin
                                (semaphore-wait *object-list-semaphore*)
                                (set! *object-list* (append *object-list* (list (new on-screen% 
                                                                                     [sprite youlosepic]
                                                                                     [x 400]
                                                                                     [y 300]))))
                                (semaphore-post *object-list-semaphore*)
                                (sleep 0.1)
                                (send new-game stop-update))))
    (define/public (throw) 
      (if (not weapon)           ;no weapon
          (let((old-power power))
            (begin
              (set! power 0)
              (new snowball% 
                   [sprite snowball-sprite]
                   [x (+ (* side (+ (get-radius) 2)) (get-x))]
                   [y (get-y)]
                   [speed (* side old-power)])))
          
          (let((old-power power)) ;with weapon
            (begin
              (set! power 5)
              (new snowball% 
                   [sprite weapon-sprite]
                   [x (+ (* side (+ (get-radius) 2)) (get-x))]
                   [y (get-y)]
                   [speed (* side old-power)])))))
      
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
    (super-new)))
        
   

(define weapon%
  (class on-screen%
    (super-new)))
    
    

;----------instances-------------

(define *player*
  (new player% 
       [sprite (make-object bitmap% "pics/red_player.png" 'png/alpha #f)]
       [powerbar (new powerbar% [sprite (make-object bitmap% "pics/kraft0.png" 'png/alpha #f)])]))

(send *player* set-radius! (round (/ (send (send *player* get-sprite) get-height) 2)))






;------------object-list-----------



(define *object-list* (list *player* (send *player* get-powerbar)))







