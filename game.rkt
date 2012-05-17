(load "gui.rkt")
(load "networking.rkt")
(load "init.rkt")
(load "helpfunctions.rkt")

;(require graphics/graphics) seems to work without it
(define *object-list-semaphore* (make-semaphore 1))

(define Game%
  (class object%
    (super-new)
    (field (WIDTH 21)
           (HEIGHT 21)
           (*update-loop* #f)
           (mouse-x 0)
           (mouse-y 0)
           (remote-player-sprite #f))
    ;--------------set-methods----------------
    (define/public (set-local-player-sprite! pic-location)
      (send *player* set-sprite! (make-object bitmap% pic-location 'png/alpha #f))
      (send *player* set-radius! (round (/ (send (send *player* get-sprite) get-height) 2))))
    
    (define/public (set-remote-player-sprite! pic-location)
      (set! remote-player-sprite (make-object bitmap% pic-location 'png/alpha #f)))
    
    ;---------------get-methods---------------
    
    (define/public (get-remote-player-sprite) remote-player-sprite)
    (define/public (get-width) WIDTH)
    (define/public (get-height) HEIGHT)
    
    
    
    (define (draw)
      (clear)
      (draw-object-list *object-list*)
      (draw-object-list (send *network* get-remote-objects)) 
      (show))
    
    (define (draw-object-list object-list)
      (for-each (lambda (object)           ;iterates through a list with all the objects and draws the objects images on the objects coordinates
                                (draw-pic (send object get-sprite)
                                          (- (send object get-x) (/ (send (send object get-sprite) get-width) 2)) 
                                          (- (send object get-y) (/ (send (send object get-sprite) get-height) 2))))
                object-list))
    
    (define (update)
      (update-snowballs)
      (update-player)
      ;(collisionhandler (append *object-list* (send *network* get-remote-objects)))
      (draw))
    
    (define (update-snowballs)
      (define templist (cons (car *object-list*) (cdr *object-list*))) ;; creates new list with the same elements as *object-list*
      (for-each (lambda (object) (if (send object move) (set! templist (remove object templist eq?)))) *object-list*)
      (semaphore-wait *object-list-semaphore*)
      (set! *object-list* templist)
      (semaphore-post *object-list-semaphore*))
   
    (define (update-player) ;; update player is in a need of comments =)
      
      (let* ((dir-v (directional-vector (send *player* get-x) (send *player* get-y) mouse-x mouse-y))
             (dir-x (car dir-v))
             (dir-y (cdr dir-v))
             (delta-x (round (* (send *player* get-speed) dir-x)))
             (delta-y (round (* (send *player* get-speed) dir-y))))
        
        (define (update-player-x!)
          (if (>= (abs (- (send *player* get-x) mouse-x)) (send *player* get-speed)) 
              (send *player* set-x! (+ (send *player* get-x) delta-x))
              (send *player* set-x! mouse-x))) ; To avoid oscillation around mouse possition
        
        (define (update-player-y!)
          (if (>= (abs (- (send *player* get-y) mouse-y)) (send *player* get-speed)) 
              (send *player* set-y! (+ (send *player* get-y) delta-y))
              (send *player* set-y! mouse-y)))
        
        (define (can-go-there? x y)
          (let ((result #t)
                (remote-objects (send *network* get-remote-objects))
                (moved-player (new player% [x (+ (send *player* get-x) x)] [y (+ (send *player* get-y) y)] [radius (send *player* get-radius)])))
            (begin 
              (for-each (lambda (object)
                          (if (not (or (is-a? object powerbar%) (eq? object *player*)))
                              (if (collision? object moved-player)
                                  (set! result #f)))) (append *object-list* remote-objects))
              result)))

          (send *player* update-powerbar!)
          (if (can-go-there? delta-x delta-y)
              (begin
                (update-player-x!)
                (update-player-y!)))))
    
    (define (directional-vector x1 y1 x2 y2)
      (let* ((x (- x2 x1))
             (y (- y2 y1))
             (length (sqrt (+ (sqr x) (sqr y)))))
        (if (= length 0) (cons 0 0) (cons (/ x length) (/ y length)))))
      
    
    
    (define/public (collisionhandler crashlist)
      
      (define (snowballcollission first-object second-object)
        (let*
            ((snowball (if (is-a? first-object snowball%) first-object second-object))
             (other-object (if (eq? snowball first-object) second-object first-object)))
          (if (occurs? snowball *object-list*)                ;is the snowball my snowball?
              (cond
                ((and (is-a? other-object player%)            ;does my snowball hit a player?
                      (not (eq? other-object *player*)))      ;does my snowball hit the opponent or me? Nothing will happen if I hit myself.
                 (begin (send *network* hit!)
                        (semaphore-wait *object-list-semaphore*)
                        (set! *object-list* (remove snowball *object-list* eq?))
                        (semaphore-post *object-list-semaphore*))) ;if so, remove the snowball
                ((is-a? other-object bunker%) (begin
                                                (semaphore-wait *object-list-semaphore*)
                                                (set! *object-list* (remove snowball *object-list* eq?))
                                                (semaphore-post *object-list-semaphore*))) ;if you hit a bunker, remove the snowball
                ((is-a? other-object snowball%) (begin
                                                (semaphore-wait *object-list-semaphore*)
                                                (set! *object-list* (remove snowball *object-list* eq?))
                                                (semaphore-post *object-list-semaphore*))))))) ;if you hit another snowball, remove the snowball
          
        
              (if (not (null? crashlist))
                  (let ((first-object (car crashlist))) 
                    (for-each  
                     (lambda (second-object) 
                       (if (collision? first-object second-object) ;will the two objects collide
                           (if (or (is-a? first-object snowball%) (is-a? second-object snowball%))
                               (snowballcollission first-object second-object))))
                     (cdr crashlist)))             
                  (collisionhandler (cdr crashlist))))
        
      
    
      
      
    (define/public (update-mouse x y)
      (set! mouse-x x)
      (set! mouse-y y))
    
    
    (define/public (exit-game)
      (hide-gui *gui*))
    
    (define/public (stop-update)
      (send *update-loop* stop))
        
    (define/public (start-update)
      (set! *update-loop* (new timer%
                             [notify-callback update]
                             [interval 20]
                             [just-once? #f])))
    
      
    (define/public (start-game)
      (show-gui *gui*)
      (draw-text "welcome to snowballfight" 350 300 *black-pen* *green-brush*)
      (draw-text "After you have either connected or started listening, press pray game!" 200 350 *black-pen* *green-brush*))))




(define new-game (new Game%))
(send new-game start-game)
