;---superclass for object that can be displayed on screen as sprite---
(define on-screen% 
  
  (class object%
    (super-new)
    
    (field (x 0) (y 0)   ;; position of object on screen 
           (sx 0) (sy 0) ;; space occupied by object on screen
           (sprite ""))  ;; path to image representing object on screen
    
    
    ;---------------set-methods-------------------
    (define/public (set-xy! new-x new-y) (begin (set! x new-x) (set! y new-y)))
    (define/public (set-x! new-x) (set! x new-x))
    (define/public (set-y! new-y) (set! y new-y))
    (define/public (set-sx! new-sx) (set! sx new-sx))
    (define/public (set-sy! new-sy) (set! sy new-sy))
    (define/public (set-sprite! new-sprite) (set! sprite new-sprite))
    
    ;;--------------get-methods-------------------
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-sx) sx)
    (define/public (get-sy) sy)
    (define/public (get-sprite) sprite)))
    
    

(define player%
  (class on-screen%
    (super-new)
    (field (hp 2)) ; hitpoints
  
    ;---------------set-methods-------------------
    (define/public (set-hp! new-hp) (set! hp new-hp))
  
    ;---------------get-methods------------------
    (define/public (get-hp) hp)
    (define/public (alive?) (not (= hp 0)))
  
    ;---------------actions----------------------
    (define/public (hit!) (if (> hp 0) (set! hp (- hp 1)) (display "Error already dead!")))
    (define/public (throw) (new snowball%))))
  


(define snowball%
  (class on-screen%
    (super-new)
    (inherit set-x! get-x)
    (field (speed 0) (distance 5)) ; speed is negative for snowballs thrown left
    
    ;---------------set-methods-----------------
    (define/public (set-throw_param! new-speed new-distance) (begin (set! speed new-speed) 
                                                             (set! distance new-distance)))
    
    (define/public (set-power! power) (begin (set! speed power) 
                                             (set! distance (abs power)))) ; likely to be modifyed
    
    ;---------------get-methods------------------
    (define/public (get-speed) speed)
    (define/public (get-distance) distance)
    
    ;---------------actions---------------------
    (define/public (move) ; Returns #t when maximum distance is reached
      (begin (set-x! (+ (get-x) speed)) 
             (set! distance (- distance 1)) (= distance 0)))


(define bunker%
  (class on-screen%
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