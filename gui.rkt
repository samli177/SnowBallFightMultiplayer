;; ---------------------------------------------------------------------
;; GUI
;; ---------------------------------------------------------------------

;; CONSTRUCTOR

(define (make-gui frame canvas buffer dc)
  (list frame canvas buffer dc))

;; SELECTORS

(define (get-frame gui)
  (car gui))

(define (get-canvas gui)
  (cadr gui))

(define (get-buffer gui)
  (caddr gui))

(define (get-dc gui)
  (cadddr gui))

(define (show-gui gui)
  (send (get-frame gui) show #t))

(define (hide-gui gui)
  (send (get-frame gui) show #f))

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
  (send dc draw-bitmap (get-buffer *gui*) 0 0))

(define (redraw)
  (send (get-canvas *gui*) on-paint))

;; ---------------------------------------------------------------------
;; Mouse function
;; ---------------------------------------------------------------------
(define (power-up!)
  (if (<= (send *player* get-power) 35) (send *player* power-up!)))

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
              (semaphore-wait sync-semaphore) ; to avoid conflict in *object-list*
              (set! *object-list* (cons (send *player* throw) *object-list*))
              (semaphore-post sync-semaphore)
              (send *player* power-down!)))
              
          ((right-down)
           (background (random 255) (random 255) (random 255)))
          ((motion)
           (send new-game update-mouse x y)))))


;; ---------------------------------------------------------------------
;; Functions to draw
;; ---------------------------------------------------------------------

; A procedure that clears the GUI
(define (clear)
  (send *dc* clear))

(define (background r g b)
  (send *dc* set-background  (make-object color% r g b)))


;; A procedures that draws text
(define (draw-text text x y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-text text x y))

;; A procedures that draws a picture from file
(define (draw-pic file x y)
  (send (get-dc *gui*) draw-bitmap file x y))

; A procedure that shows the new GUI
(define (show)
  (redraw))

;; The colors to draw with:
(define *red-pen* 
  (send the-pen-list find-or-create-pen "red" 4 'solid))
(define *green-pen* 
  (send the-pen-list find-or-create-pen "green" 2 'solid))
(define *black-pen* 
  (send the-pen-list find-or-create-pen "black" 2 'solid))
(define *blue-pen* 
  (send the-pen-list find-or-create-pen "blue" 2 'solid))
(define *yellow-pen* 
  (send the-pen-list find-or-create-pen "yellow" 2 'solid))
(define *white-pen* 
  (send the-pen-list find-or-create-pen "white" 2 'solid))

(define *yellow-brush* 
  (send the-brush-list find-or-create-brush "yellow" 'solid))
(define *red-brush* 
  (send the-brush-list find-or-create-brush "red" 'solid))
(define *blue-brush* 
  (send the-brush-list find-or-create-brush "blue" 'solid))
(define *green-brush* 
  (send the-brush-list find-or-create-brush "green" 'solid))
(define *white-brush* 
  (send the-brush-list find-or-create-brush "white" 'solid))
(define *black-brush* 
  (send the-brush-list find-or-create-brush "black" 'solid))



;; --------------------------------------------------------------------
;; The GUI and its components (buttons, menus etc)
;; --------------------------------------------------------------------

(define *frame* (make-object frame% "Samuels och Bens datorspel"))


(define *menu-bar* 
  (instantiate menu-bar%
    (*frame*)))

(define *menu* 
  (instantiate menu%
    ("Menu" *menu-bar*)))

(instantiate menu-item%
  ("Listen" *menu* (lambda (a b) (send *network* listen))))

(instantiate menu-item%
  ("Connect" *menu* (lambda (a b) (begin 
                                    (send *player* set-side! -1)
                                    (send *network* set-host! (get-text-from-user "Connect" "Enter target IP:"))
                                    (send *network* connect)))))

(instantiate menu-item%
  ("Pray Game" *menu* (lambda (a b) (send new-game start-update))))

(instantiate menu-item%
  ("Quit" *menu* (lambda (a b) (hide-gui *gui*))))

(define *canvas*
  (instantiate my-canvas% ()
    (parent *frame*)
    (paint-callback draw-canvas)
    (mouse-callback mouse-fn)
    (min-width 700)
    (min-height 500)
    (stretchable-width #f) 
    (stretchable-height #f)))

(define *buffer* (make-object bitmap% 700 500 #f))
(define *dc* (make-object bitmap-dc% *buffer*))

(define *gui* 
  (make-gui 
   *frame*
   *canvas*
   *buffer*
   *dc*))


