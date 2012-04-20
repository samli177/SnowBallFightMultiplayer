(load "martingui.rkt")
(require graphics/graphics)

(define Game%
  (class object%
    (super-new)
    (field (WIDTH 21)
           (HEIGHT 21)
           (*should-run* #f)
           (image null))
    
    (define/public (get-width) WIDTH)
    (define/public (get-height) HEIGHT)
    
    (define (init)
      (set! image (make-object bitmap% "testbild.jpg" 'unknown #f))
      (background 0 100 0))
    
    (define (draw)
      (clear)
      (draw-pic image 0 0)
      ;(draw-pic gubbe gubbex gubbey) När vi har en gubbe med tillhörande koordinater ritas den ut
      (show)
      ;(draw-text query-mouse-posn 200 200) query-mouse-posn ska nog inte användas
      )
    
    (define (update)
      (draw))
    
       
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
      (init)
      (start-update)
      )
    )
  )

(define new-game (new Game%))
(send new-game start-game)
