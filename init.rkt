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
    
    ;--------------set-methods----------------
    (define/public (set-power! new-power) (begin (set! power new-power) 
                                                 (case type
                                                   ((eq? power 0) (set-sprite! "kraft0.png"))
                                                   ((eq? power 1) (set-sprite! "kraft1.png"))
                                                   ((eq? power 2) (set-sprite! "kraft2.png"))
                                                   ((eq? power 3) (set-sprite! "kraft3.png"))
                                                   ((eq? power 4) (set-sprite! "kraft4.png"))
                                                   ((eq? power 5) (set-sprite! "kraft5.png")))))
                                                   
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
                (powerbar #f))
  
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
  
    ;---------------actions----------------------
    (define/public (power-up!) (set! power (+ 1 power)))
    (define/public (power-down!) (set! power 0))
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) (display "Error already dead!")))
    (define/public (throw) (new snowball% 
                                [sprite (make-object bitmap% "snowball.png" 'png/alpha #f)]
                                [x (* (get-side) (+ (get-radius) (get-x) 2))]
                                [y (get-y)]
                                [speed (* side power)]))
    (define/public (update-powerbar!) (if (not (eq? power (send powerbar get-power))) (send powerbar set-power! power))))) 

                                
    
    

  


(define snowball%
  (class on-screen%
    (super-new)
    (inherit set-x! get-x)
    (init-field (speed 10) (distance 100) (counter 0)) ; speed is negative for snowballs thrown left
    
    ;---------------set-methods-----------------
    (define/public (set-throw_param! new-speed new-distance) (begin (set! speed new-speed) 
                                                                    (set! distance new-distance)))
    
    (define/public (set-power! power) (begin (set! speed power) 
                                             (set! distance (abs power)))) ; likely to be modifyed
    
    ;---------------get-methods------------------
    (define/public (get-speed) speed)
    (define/public (get-distance) distance)
    
    ;---------------actions---------------------
    (define/override (move) ; Returns #t when maximum distance is reached
      (begin 
        (if (and (> speed 0)(= 10 counter)) (begin (set! speed (- speed 1)) (set! counter 0)))
        (if (and (< speed 0)(= 10 counter)) (begin (set! speed (+ speed 1)) (set! counter 0)))
        (set! counter (+ counter 1))
        (set-x! (+ (get-x) speed)) 
        (set! distance (- distance 1)) (= distance 0)))))


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
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) (display "Error already broken!")))))
  
    
;---------testing--------------
(define *test-osc* (new on-screen%))
(send *test-osc* set-xy! 1 2)

(define *test-pla* (new player%))

(define *test-snow* (new snowball%))

(define *test-bunker* (new bunker%))

;----------instances-------------

(define *player*
  (new player% [sprite (make-object bitmap% "blagubbe.png" 'png/alpha #f)]))
(send *player* set-radius! (round (/ (send (send *player* get-sprite) get-width) 2)))

(define *bunker*
  (new bunker% 
       [sprite (make-object bitmap% "bunker.png" 'png/alpha #f)]
       [x 300]
       [y 300]))
(send *bunker* set-radius! (round (/ (send (send *bunker* get-sprite) get-width) 2)))

(define *powerbar*
  (new powerbar% [sprite (make-object bitmap% "kraft0.png" 'png/alpha #f)]))


(define *network* (new network-session%))


;------------pictures-----------



(define *object-list* (cons *player* null))





