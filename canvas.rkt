



;(define *frame* (make-object frame% "Spelplan"))  *****Provar med koden under istället******

;(define *canvas* (new game-canvas% [parent *frame*] ;;en canvas           
;[label "tjenare"]))

(define game-canvas%
  (class canvas%
    (inherit get-width
             get-height
             refresh)
    (init [keyboard-handler display]
          [mouse-handler display])
    (define on-key-handle keyboard-handler)
    (define on-mouse-handle mouse-handler)
    ;(define/override (on-char ke)
    ;(on-key-handle ke))
    ;(define/override (on-event me)
    ;(on-mouse-handle me))
    (super-new)))



(define my-window (new frame% [label "Spelplan"])) 
(define my-canvas (new game-canvas%
                       ;[keyboard-handler keyboard-fn]
                       ;[mouse-handler mouse-fn]
                       [parent my-window]))
;[paint-callback render-fn]))
(define (render-fn canvas dc)
  
  ;; Helpfn asks every object to render itself
  (define (render-one obj)
    (send obj render dc))
  (for-each render-one *all-objects*))
(send my-window show #t)




(define agent%
  (class object%
    (init initial-x ...)
    (define x initial-x)
    ;...
    (define/public (render dc)
      (send dc translate x y)
      (send dc rotate angle)
      (send dc draw-bitmap img 0 0)
      (send dc rotate (- angle))
      (send dc translate (- x) (- y)))
    ;...
    (super-new)))

(define (update)
  (lambda () (begin
               (send my-canvas refresh)
               (draw-pic "testbild.jpg" 0 0))))

(define my-timer ;rackets timer% kan sköta spelloopen
  (new timer%
       [interval 1000] ;16ms är ungefär 60FPS
       [notify-callback update]
       [just-once? #f])) 





;; A procedures that draws a picture from file
(define (draw-pic file x y)
  (send (send *canvas* get-dc) draw-bitmap (make-object bitmap% file 'unknown #f) x y))



;; lägger in en bild på koordinater x,y. Bilden bör ligga i samma mapp som canvas.rkt.
(define (draw-pic file x y)
  (send (send my-canvas get-dc) draw-bitmap (make-object bitmap% file 'unknown #f) x y))

;; skriv t.ex (draw-pic "testbild.jpg" 10 20)