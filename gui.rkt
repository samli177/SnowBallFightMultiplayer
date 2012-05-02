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
(define (mouse-fn mouse-event)
      (let ((x (send mouse-event get-x))
            (y (send mouse-event get-y))
            (type (send mouse-event get-event-type)))
        (case type
          ((leave) null)
          ((left-down)
           (begin
             (semaphore-wait sync-semaphore)
             (set! *object-list* (cons (send *player* throw) *object-list*)))
           (semaphore-post sync-semaphore));if this goes slow, try change to mcons instead of cons
          ;left mouse click causes player to throw, and adds the object snowball in the *object-list*
          ((right-down)
           (background 0 0 0))
          ((motion)
           (send new-game update-mouse x y)))))

;; ---------------------------------------------------------------------
;; Functions to draw
;; ---------------------------------------------------------------------

; A procedure that clears the GUI
(define (clear)
  (send *dc* clear))

; A procedure that sets the background color of the GUI
(define (background)
  (send *dc* set-background  (make-object color% (random 255) (random 255) (random 255))))

(define (background r g b)
  (send *dc* set-background  (make-object color% r g b)))

;; A procedures that draws an ellipse
(define (draw-circle x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-ellipse x y size-x size-y))

;; A procedures that draws a rectangle
(define (draw-rectangle x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-rectangle x y size-x size-y))

;; A procedures that draws a line
(define (draw-line x y size-x size-y pen brush)
  (send (get-dc *gui*) set-pen pen)
  (send (get-dc *gui*) set-brush brush)
  (send (get-dc *gui*) draw-line x y (+ x size-x) (+ y size-y)))

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

(define *frame* (make-object frame% "Ben is awesome"))


(define *menu-bar* 
  (instantiate menu-bar%
    (*frame*)))

(define *menu* 
  (instantiate menu%
    ("Menu" *menu-bar*)))

(instantiate menu-item%
  ("Quit" *menu* (lambda (a b) (hide-gui *gui*))))

(define *canvas*
  (instantiate my-canvas% ()
    (parent *frame*)
    (paint-callback draw-canvas)
    (mouse-callback mouse-fn)
    (min-height 672)
    (min-width 672)
    (stretchable-width #f) 
    (stretchable-height #f)))

(define *buffer* (make-object bitmap% 832 832 #f))
(define *dc* (make-object bitmap-dc% *buffer*))

(define *gui* 
  (make-gui 
   *frame*
   *canvas*
   *buffer*
   *dc*))


