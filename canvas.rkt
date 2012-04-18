



(define *frame* (make-object frame% "Spelplan"))

(define *canvas* (new canvas% [parent *frame*] ;;en canvas 
                        [label "tjenare"]))

;; A procedures that draws a picture from file
(define (draw-pic file x y)
  (send (send *canvas* get-dc) draw-bitmap (make-object bitmap% file 'unknown #f) x y))

(send *frame* show #t)

;; skriv detta för att lägga in en bild på koordinater 
;;(define (draw-pic file x y)
;;(send (send *canvas* get-dc) draw-bitmap (make-object bitmap% file 'unknown #f) x y))