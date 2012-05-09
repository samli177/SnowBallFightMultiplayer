(load "gui.rkt")
(load "init.rkt")
(load "networking.rkt")
(load "helpfunctions.rkt")

(define sync-semaphore (make-semaphore 1))
;(require graphics/graphics) seems to work without it

(define Game%
  (class object%
    (super-new)
    (field (WIDTH 21)
           (HEIGHT 21)
           (*should-run* #f)
           (mouse-x 0)
           (mouse-y 0))
    
    (define/public (get-width) WIDTH)
    (define/public (get-height) HEIGHT)
    
    
    
    (define (draw)
      (clear)
      (draw-object-list *object-list*)
      (draw-object-list (get-remote-objects))
      ;(draw-pic *image* mouse-x mouse-y);(draw-pic character characterx charactery). Draws a picture where the mouse is. 
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
      (collisionhandler (append *object-list* (get-remote-objects)))
      (draw))
    
    (define (update-snowballs)
      (define templist (cons (car *object-list*) (cdr *object-list*))) ;; creates new list with the same elements as *object-list*
      (for-each (lambda (object) (if (send object move) (set! templist (remove object templist eq?)))) *object-list*)
      (semaphore-wait sync-semaphore)
      (set! *object-list* templist)
      (semaphore-post sync-semaphore))
   
    (define (update-player)
      (let* ((dir-v (directional-vector (send *player* get-x) (send *player* get-y) mouse-x mouse-y))
            (dir-x (car dir-v))
            (dir-y (cdr dir-v)))
        
        (if (>= (abs (- (send *player* get-x) mouse-x)) (send *player* get-speed)) 
            (send *player* set-x! (+ (send *player* get-x) (round (* (send *player* get-speed) dir-x))))
            (send *player* set-x! (+ (send *player* get-x) (round dir-x))))
        
        (if (>= (abs (- (send *player* get-y) mouse-y)) (send *player* get-speed)) 
            (send *player* set-y! (+ (send *player* get-y) (round (* (send *player* get-speed) dir-y))))
            (send *player* set-y! (+ (send *player* get-y)(round dir-y))))))
    
    (define (directional-vector x1 y1 x2 y2)
      (let* ((x (- x2 x1))
             (y (- y2 y1))
             (length (sqrt (+ (sqr x) (sqr y)))))
        (if (= length 0) (cons 0 0) (cons (/ x length) (/ y length)))))
      
    (define/public (collisionhandler crashlist) 
      (if (not (null? crashlist))
          (begin (let ((paranoid-object (car crashlist))) 
                   (for-each  
                    (lambda (aggressive-object) 
                      (if (>= (+ (send aggressive-object get-radius) (send paranoid-object get-radius))
                              (distance paranoid-object aggressive-object)) ;combined radius of two objects
                          ;(display "BANG! ")
                          (if (and (is-a? paranoid-object player%) (occur? paranoid-object (get-remote-objects)) (is-a? aggressive-object snowball%)) (display "den andra datorns spelar är träffad"))
                          ))
                    (cdr crashlist))             
                   (collisionhandler (cdr crashlist))))))
    
        
    (define/public (update-mouse x y)
      (set! mouse-x x)
      (set! mouse-y y))
    
   
       
    (define/public (pause-update)
      (set! *should-run* #f))
    
    (define/public (exit-game)
      (pause-update)
      (hide-gui *gui*))
    
    (define/public (start-update)
      (when (not *should-run*)
        (set! *should-run* #t)
        (new timer%
             [notify-callback update]
             [interval 20]
             [just-once? #f])
        (show-gui *gui*)))
    
    (define/public (start-game)
      (start-update))))



(define new-game (new Game%))
(send new-game start-game)
