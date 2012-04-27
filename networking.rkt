

(define *host* "localhost")
(define *port* 9000)

(define *inport* null)
(define *outport* null)

(define remote-string "")

(define  (set-host! newhost)
  (set! *host* nowhost))

(define (listen)
  (let ((listener (tcp-listen *port* 1 #t)))
    (set!-values (*inport* *outport*) (tcp-accept listener))
    (thread listen-for-data)))

(define (connect) 
  (set!-values (*inport* *outport*) (tcp-connect *host* *port*))
  (thread listen-for-data))


(define (listen-for-data)
  (define (loop)
    (set! remote-string (read-line *inport* 'any))
    (display remote-string)
    (loop))
  (loop))
  
(define (send string)
  (display string *outport*)
  (newline *outport*)
  (display "" *outport*))

  
(define (first-word charlist)
      (if (or (null? charlist) (equal? (car charlist) #\space))
          ""
          (string-append (string (car charlist)) (first-word (cdr charlist)))))

(define (string->symbols string)
  (let ((current-word ""))
    (define (st->symbs str)
      (begin
        (set! current-word (first-word (string->list str)))
        (cond
          ((= (string-length current-word) (string-length str)) (cons (string->symbol current-word) '()))
          ((equal? current-word "") '())
          (else (cons (string->symbol current-word) (st->symbs (substring str (+ 1 (string-length current-word)))))))))
    (st->symbs string)))
        
