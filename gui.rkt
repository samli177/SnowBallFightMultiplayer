;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------
(define gui-interface%
  (class object%
    (super-new)
    
    
    
    ;; CONSTRUCTOR
    
    (define (make-gui frame canvas buffer dc) ;dc means drawing context
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
    
     ;; ------------------------------------------------------------------------
    ;; The GUI and its components (buttons, menus etc)
    ;; -------------------------------------------------------------------------
    
    (define frame (make-object frame% "Samuels och Bens datorspel"))
    
    (define menu-bar           ;adds a bar to the frame
      (instantiate menu-bar%
        (frame)))
    
    (define menu               ;adds the menu-button to the bar
      (instantiate menu%
        ("Menu" menu-bar)))
    
    (instantiate menu-item% ;adds the choice "listen" to the menu-buttons list
      ("Listen" menu 
                (lambda (a b) 
                  (begin
                    (send new-game set-local-player-sprite! "red_player.png")
                    (send new-game set-remote-player-sprite! "blue_player.png")
                    (send *player* set-xy! 60 300)     ;coordinates for spawning
                    (send (send new-game get-network) listen)))))
    
    (instantiate menu-item%  ;adds the choise "connect" to the menu-buttons list
      ("Connect" menu 
                 (lambda (a b) 
                   (begin
                     (send new-game set-local-player-sprite! "blue_player.png")
                     (send new-game set-remote-player-sprite! "red_player.png")
                     (send *player* set-xy! 1140 300) ;coordinates for spawning
                     (send new-game bunkeradder 
                           (string->number (get-text-from-user 
                                            "Please enter how many bunkers you 
want on the battle field" "Type a number and press ok or enter")))
                     (send *player* set-side! -1)    ;throw balls to the left
                     (send (send new-game get-network) set-host! 
                           (get-text-from-user "Connect" "Enter target IP:"))
                     (send (send new-game get-network) connect)))))
    
    (instantiate menu-item%;adds the choise "Pray Game" to the menu-buttons list
      ("Pray Game" menu (lambda (a b) (send new-game start-update))))
    
    (instantiate menu-item%    ;adds the choise "quit" to the menu-buttons list
      ("Quit" menu (lambda (a b) (hide-gui ))))
    
    
    
    
    
    
    ;; -------------------------------------------------------------------------
    ;; Mouse function
    ;; -------------------------------------------------------------------------
    (define (power-up!)                ;helpfunction to power-timer
      (send *player* power-up!))
    
    (define power-timer (new timer%    ;makers players power rise when activated
                             [notify-callback power-up!]
                             [interval #f]
                             [just-once? #f]))
    
    (define (mouse-fn mouse-event) ;Different actions for different mouse events
      (let ((x (send mouse-event get-x))
            (y (send mouse-event get-y))
            (type (send mouse-event get-event-type)))
        (case type
          ((leave) null)         ;nothing happens if the mouse leaves the screen
          ((left-down)
           (send power-timer start 40))  
          ;When you press left mouse button the players power begins to rise
          
          ((left-up)
           (begin
             (send power-timer stop)    
             ;The power stops rising whe you release the left mouse button
            
             (semaphore-wait *object-list-semaphore*) 
             ;to avoid conflict in *object-list*
            
             (set! *object-list* (cons (send *player* throw) *object-list*))
             ;A snowball is thrown
            
             (semaphore-post *object-list-semaphore*)
             (send *player* power-down!)))    ;The players power returns to zero
          ((right-down)
           (background (random 255) (random 255) (random 255)))
          ((motion)
           (send new-game update-mouse x y)))))
    
    ;; -------------------------------------------------------------------------
    ;; Canvas
    ;; -------------------------------------------------------------------------
    
    
    (define my-canvas% 
      (class canvas%
        (override on-char)
        (override on-event)
        (init-field (key-callback #f))
        (init-field (mouse-callback #f))
        (define (on-char event);Not used now but makes it easy to add use of keyboard
          (when key-callback
            (key-callback event)))
        (define (on-event event)         
          (when mouse-callback
            (mouse-callback event)))
        (super-instantiate ())))
    
    
    (define (draw-canvas canvas dc) ;draws the canvas
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
    ;calls back to paint-callback wich is associated with draw-canvas
    
    
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
    
    