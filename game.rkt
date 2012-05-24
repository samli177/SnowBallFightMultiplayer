(load "gui.rkt")
(load "networking.rkt")
(load "init.rkt")
(load "helpfunctions.rkt")

;Commenting is in first hand read on the same line as code.
;Otherwise above a use of "define" or if the comments are in 
;the middle of a code, they will be found directly below the commented code.


(define *object-list-semaphore* (make-semaphore 1))

(define Game%
  (class object%
    (super-new)
    (field (WIDTH 21)
           (HEIGHT 21)
           (update-loop #f)
           (mouse-x 0)
           (mouse-y 0)
           (background-image 
            (make-object bitmap% "pics/background.png" 'png/alpha #f))
           (startscreen 
            (make-object bitmap% "pics/startscreen.png" 'png/alpha #f))
           (red_playerweapon 
            (make-object bitmap% "pics/red_playerweapon.png" 'png/alpha #f))
           (blue_playerweapon 
            (make-object bitmap% "pics/blue_playerweapon.png" 'png/alpha #f))
           (gui (new gui-interface%))
           (update-semaphore (make-semaphore 1))
           (network (new network-session%)))
    
    ;--------------set-methods-------------------------------------------------
    (define/public (set-local-player-sprite! pic-location)
      (send *player* set-sprite! (make-object bitmap% pic-location 'png/alpha #f))
      (send *player* set-radius! 
            (round (/ (send (send *player* get-sprite) get-height) 2))))
    
    (define/public (set-remote-player-sprite! pic-location)
      (send network set-player-sprite! pic-location))
    
    
    ;---------------get-methods-------------------------------------------------
    
    (define/public (get-remote-player-sprite) remote-player-sprite)
    (define/public (get-remote-player-radius) remote-player-radius)
    (define/public (get-width) WIDTH)
    (define/public (get-height) HEIGHT)
    (define/public (get-network) network)
    
    ;--------------update functions---------------------------------------------
    
    (define (update)
      (semaphore-wait update-semaphore)
      (update-snowballs)
      (update-player)
      (collisionhandler (append *object-list* (send network get-remote-objects)))
      (draw)
      (semaphore-post update-semaphore))
    
    (define (update-snowballs)
      (define templist (cons (car *object-list*) (cdr *object-list*)))
      ; creates new list with the same elements as *object-list*
      
      (for-each (lambda (object) 
                  (if (send object move) 
                      (set! templist (remove object templist eq?)))) 
                *object-list*)
      (semaphore-wait *object-list-semaphore*)
      (set! *object-list* templist)
      (semaphore-post *object-list-semaphore*))
    
    (define (update-player) 
      ;makes a normalized vector that points from the player to the mouse
      (let* ((dir-v (directional-vector 
                     (send *player* get-x) (send *player* get-y) mouse-x mouse-y))
             (dir-x (car dir-v))
             (dir-y (cdr dir-v))
             (delta-x (round (* (send *player* get-speed) dir-x)))
             (delta-y (round (* (send *player* get-speed) dir-y))))
        
        (define (update-player-x!)
          ;moves the players x coordinate according to the vector
          (if (>= (abs (- (send *player* get-x) mouse-x)) 
                  (send *player* get-speed)) 
              (send *player* set-x! (+ (send *player* get-x) delta-x))
              (send *player* set-x! mouse-x))) 
        ;To avoid oscillation around mouse position
        
        (define (update-player-y!)
          ;moves the players y coordinate according to the vector
          (if (>= (abs (- (send *player* get-y) mouse-y))
                  (send *player* get-speed)) 
              (send *player* set-y! (+ (send *player* get-y) delta-y))
              (send *player* set-y! mouse-y)))
        ;To avoid oscillation around mouse position
        
        (define (can-go-there? x y)
          (let ((result #t)
                (remote-objects (send network get-remote-objects))
                (moved-player (new player% 
                                   [x (+ (send *player* get-x) x)]
                                   [y (+ (send *player* get-y) y)] 
                                   [radius (send *player* get-radius)])))
            (begin 
              (for-each (lambda (object)
                          (if (not (or (is-a? object powerbar%)
                                       (eq? object *player*) 
                                       (is-a? object weapon%)))
                              (if (collision? object moved-player)
                                  (set! result #f)))) 
                        (append *object-list* remote-objects))
              result)))
        ;Aunction that makes movement into objects impossible, 
        ;apart from youself, weapon on the ground and your powerbar.
        
        (send *player* update-powerbar!)
        (if (can-go-there? delta-x delta-y)
            (begin
              (update-player-x!)
              (update-player-y!)))))
        
    
    ;-------functions to add items to the game----------------------------------
    
    (define/public (bunkeradder number) ;adds bunkers to the battle field
      (let* ((sprite (make-object bitmap% "pics/bunker.png" 'png/alpha #f))
             (radius (/ (send sprite get-height) 2))
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
    
    (define (weaponadder)
      (let* ((sprite (make-object bitmap% "pics/weaponbox.png" 'png/alpha #f))
             (radius (send sprite get-height))
             (generated-x (+ 300 (random 600)))
             (generated-y (+ 100 (random 300))))
        (begin (sleep (+ 10 (random 30))) (set! *object-list* (cons (new weapon%
                                              [sprite sprite]
                                              [radius radius]
                                              [x generated-x]
                                              [y generated-y])
                                         *object-list*)))))
    
    (define/public (start-weapon-adder)
      (thread weaponadder))
  
    
    
    (define (draw)
      (send gui clear)
      (send gui draw-pic background-image 0 0)
      (draw-object-list *object-list*)
      (draw-object-list (send network get-remote-objects)) 
      (send gui show))
    
    (define (draw-object-list object-list)
      (for-each (lambda (object)           
                  (send gui draw-pic (send object get-sprite)
                        (- (send object get-x) 
                           (/ (send (send object get-sprite) get-width) 2)) 
                        (- (send object get-y) 
                           (/ (send (send object get-sprite) get-height) 2))))
                object-list))
    ;iterates through a list with all the objects and draws the objects images
    ;on the objects coordinates
    
    
    
    (define (directional-vector x1 y1 x2 y2)
      (let* ((x (- x2 x1))
             (y (- y2 y1))
             (length (sqrt (+ (sqr x) (sqr y)))))
        (if (= length 0) (cons 0 0) (cons (/ x length) (/ y length)))))
    
    (define/public (win!)
      (set! *object-list* 
            (append *object-list* 
                    (list 
                     (new on-screen% 
                          [sprite 
                           (make-object bitmap% 
                             "pics/youwinpic.png" 'png/alpha #f)]
                          [x 400]
                          [y 300])))))
    
    ;-----------Function to handle collissions----------------------------------
    
    (define (collisionhandler crashlist)
      
      (define (snowballcollission first-object second-object)
        (let*
            ((snowball (if 
                        (is-a? first-object snowball%)
                        first-object 
                        second-object))
             (other-object (if 
                            (eq? snowball first-object)
                               second-object 
                               first-object)))
          (if (occurs? snowball *object-list*) ;is the snowball my snowball?
              (cond
                ((and (is-a? other-object player%)           
                      ;does my snowball hit a player?
                      
                      (not (eq? other-object *player*)))     
                 ;does my snowball hit the opponent or me? 
                 ;Nothing will happen if I hit myself.
                 
                 (begin (send network hit!)
                        (semaphore-wait *object-list-semaphore*)
                        (set! *object-list* (remove snowball *object-list* eq?))
                        (semaphore-post *object-list-semaphore*))) 
                ;if so, remove the snowball
                ((is-a? other-object bunker%) 
                 (begin
                   (semaphore-wait *object-list-semaphore*)
                   (set! *object-list* (remove snowball *object-list* eq?))
                   (semaphore-post *object-list-semaphore*)))
                ;if you hit a bunker, remove the snowball
                ((is-a? other-object snowball%)
                 (begin
                   (semaphore-wait *object-list-semaphore*)
                   (set! *object-list* (remove snowball *object-list* eq?))
                   (semaphore-post *object-list-semaphore*))))))) 
      ;if you hit another snowball, remove the snowball
      
      (define (weaponcollission first-object second-object)
        (let*
            ((weapon (if (is-a? first-object weapon%) first-object second-object))
             (other-object 
              (if (eq? weapon first-object) second-object first-object)))
          (if (and (is-a? other-object player%)
                   (occurs? other-object *object-list*))
              ;does the weapon collide with a player? And is that player me?
              (begin (send *player* set-weapon! weapon) ;the player gets the weapon
                     (send *player* set-sprite! (if (= 1 (send *player* get-side)) 
                                                ;change sprite according to side
                                                    red_playerweapon 
                                                    blue_playerweapon))
                     (if (occurs? weapon *object-list*) 
  ;is the weapon in my object-list or does it come from the other players list? 
                         (begin 
                           (remove-weapon!) (send network weapon-is-taken!))             
      ;remove the weapon and set sprites
                         (send network weapon-is-taken!)))))) 
      ;or tell the other computer to do it
      
      (if (not (null? crashlist))
          (let ((first-object (car crashlist))) 
            (for-each  
             (lambda (second-object) 
               (if (collision? first-object second-object) 
                   ;will the two objects collide?
                   (cond 
                     ((or (is-a? first-object snowball%)
                          (is-a? second-object snowball%))
                      (snowballcollission first-object second-object))
                     ((or (is-a? first-object weapon%) 
                          (is-a? second-object weapon%))
                      (weaponcollission first-object second-object)))))
             (cdr crashlist)))             
          (collisionhandler (cdr crashlist))))
    
    
    (define/public (remove-weapon!)
      (semaphore-wait *object-list-semaphore*)
      (if (is-a? (car *object-list*) weapon%)
          (set! *object-list* (cdr *object-list*))
          (set! *object-list* (remove weapon% *object-list* is-a?)))
      (semaphore-post *object-list-semaphore*))
    ;remove can ommits the car in a list, hence the if 
    
    (define/public (update-mouse x y)
      (set! mouse-x x)
      (set! mouse-y y))
    
    (define/public (stop-update)
      (send update-loop stop))
    
    (define/public (start-update)
      (set! update-loop (new timer%
                               [notify-callback update]
                               [interval 20]
                               [just-once? #f])))
    
    
    (define/public (start-game)
      (send gui show-gui)
      (send gui draw-pic startscreen 0 0)
      (void))))
    




(define new-game (new Game%))
(send new-game start-game)
