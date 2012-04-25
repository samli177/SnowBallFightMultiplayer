(load "martingui.rkt")
(load "init.rkt")
;(require graphics/graphics) seems to work without it

(define Game%
  (class object%
    (super-new)
    (field (WIDTH 21)
           (HEIGHT 21)
           (*should-run* #f)
           (mouse-x 0)
           (mouse-y 0)
           )
    
    (define/public (get-width) WIDTH)
    (define/public (get-height) HEIGHT)
    
    
    
    (define (draw)
      (clear)
      (draw-pic *image* mouse-x mouse-y);(draw-pic character characterx charactery). Draws a picture where the mouse is. 
      (show)
      )
    
    (define (update)
      (send *player* set-xy! mouse-x mouse-y)
      (draw))
    
    (define/public (update-mouse x y)
      (set! mouse-x x)
      (set! mouse-y y))
    
       
    (define/public (pause-update)
      (set! *should-run* #f)
      )
    
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
      (start-update)
      )
    )
  )

(define new-game (new Game%))
(send new-game start-game)
