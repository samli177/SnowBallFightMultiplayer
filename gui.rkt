;; ---------------------------------------------------------------------
;; GUI
;; ---------------------------------------------------------------------
(define gui-interface%
  (class object%
    (super-new)
    
    ;;
    
    
    ;; CONSTRUCTOR
    
    (define (make-gui frame canvas buffer dc)
      (list frame canvas buffer dc))
    
    ;; SELECTORS
    
    (define (get-frame)
      (car gui))
    
    (define (get-canvas)
      (cadr gui))
    
    (define (get-buffer)
      (caddr gui))
    
    (define (get-dc)
      (cadddr gui))
    
    (define/public (show-gui)
      (send (get-frame) show #t))
    
    (define (hide-gui)
      (send (get-frame) show #f))
    
    (define/public (get-pen) black-pen)
    (define/public (get-brush) black-brush)
    
     ;; --------------------------------------------------------------------
    ;; The GUI and its components (buttons, menus etc)
    ;; --------------------------------------------------------------------
    
    (define frame (make-object frame% "Samuels och Bens datorspel"))
    
    
    (define menu-bar 
      (instantiate menu-bar%
        (frame)))
    
    (define menu 
      (instantiate menu%
        ("Menu" menu-bar)))
    
    (instantiate menu-item%
      ("Listen" menu (lambda (a b) (begin
                                       (send new-game set-local-player-sprite! "red_player.png")
                                       (send new-game set-remote-player-sprite! "blue_player.png")
                                       (send *player* set-xy! 60 300) ;coordinates for spawning
                                       (send (send new-game get-network) listen)))))
    
    (instantiate menu-item%
      ("Connect" menu (lambda (a b) (begin
                                        (send new-game set-local-player-sprite! "blue_player.png")
                                        (send new-game set-remote-player-sprite! "red_player.png")
                                        (send *player* set-xy! 1140 300) ;coordinates for spawning
                                        (send new-game bunkeradder (string->number (get-text-from-user "Please enter how many bunkers you want on the battle field" "Type a number and press ok or enter")))
                                        (send *player* set-side! -1)    ;throw balls to the left
                                        (send (send new-game get-network) set-host! (get-text-from-user "Connect" "Enter target IP:"))
                                        (send (send new-game get-network) connect)))))
    
    (instantiate menu-item%
      ("Pray Game" menu (lambda (a b) (send new-game start-update))))
    
    (instantiate menu-item%
      ("Quit" menu (lambda (a b) (hide-gui ))))
    
    
    
    
    
    
     ;; ---------------------------------------------------------------------
    ;; Mouse function
    ;; ---------------------------------------------------------------------
    (define (power-up!)
      (send *player* power-up!))
    
    (define power-timer (new timer%
                             [notify-callback power-up!]
                             [interval #f]
                             [just-once? #f]))
    
    (define (mouse-fn mouse-event)
      (let ((x (send mouse-event get-x))
            (y (send mouse-event get-y))
            (type (send mouse-event get-event-type)))
        (case type
          ((leave) null)
          ((left-down)
           (send power-timer start 40))
          
          ((left-up)
           (begin
             (send power-timer stop)
             (semaphore-wait *object-list-semaphore*) ; to avoid conflict in *object-list*
             (set! *object-list* (cons (send *player* throw) *object-list*))
             (semaphore-post *object-list-semaphore*)
             (send *player* power-down!)))
          ((right-down)
           (background (random 255) (random 255) (random 255)))
          ((motion)
           (send new-game update-mouse x y)))))
    
    ;; ---------------------------------------------------------------------
    ;; Canvas
    ;; ---------------------------------------------------------------------
    
    
    (define my-canvas% 
      (class canvas%
        (override on-char)
        (override on-event)
        (init-field (key-callback #f))
        (init-field (mouse-callback #f))
        (define (on-char event)
          (when key-callback
            (key-callback event)))
        (define (on-event event) 
          (when mouse-callback
            (mouse-callback event)))
        (super-instantiate ())))
    
    
    (define (draw-canvas canvas dc)
      (send dc draw-bitmap (get-buffer) 0 0))
    
    (define canvas
      (instantiate my-canvas% ()
        (parent frame)
        (paint-callback draw-canvas)
        (mouse-callback mouse-fn)
        (min-width 1200)
        (min-height 600)
        (stretchable-width #f) 
        (stretchable-height #f)))
    
    
    
    (define (redraw)
      (send (get-canvas) on-paint))
    
    
    
    (define buffer (make-object bitmap% 1200 600 #f))
    (define dc (make-object bitmap-dc% buffer))
    
    (define gui 
      (make-gui 
       frame
       canvas
       buffer
       dc))
    
   
    
    
    ;; ---------------------------------------------------------------------
    ;; Functions to draw
    ;; ---------------------------------------------------------------------
    
    ; A procedure that clears the GUI
    (define/public (clear)
      (send dc clear))
    
    (define (background r g b)
      (send dc set-background  (make-object color% r g b)))
    
    
    ;; A procedures that draws text
    (define/public (draw-text text x y pen brush)
      (send (get-dc) set-pen pen)
      (send (get-dc) set-brush brush)
      (send (get-dc) draw-text text x y))
    
    ;; A procedures that draws a picture from file
    (define/public (draw-pic file x y)
      (send (get-dc) draw-bitmap file x y))
    
    ; A procedure that shows the new GUI
    (define/public (show)
      (redraw))
    
    ;; The colors to draw with:
    
    (define black-pen 
      (send the-pen-list find-or-create-pen "black" 2 'solid))
    
    (define black-brush 
      (send the-brush-list find-or-create-brush "black" 'solid))))
    
    