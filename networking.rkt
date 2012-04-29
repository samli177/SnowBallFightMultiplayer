(load "table-helper.scm")
;----------variables------------
(define *host* "192.168.1.104")
(define *port* 9000)

(define *inport* null)
(define *outport* null)

(define *remote-word-list* '())
(define *remote-object-list* '())
(define *outgoing-object-list* '())

(define *rol-semaphore* (make-semaphore 1))

;--------actual networking-stuff-------
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
    (set! *remote-word-list* (read-line *inport* 'any))
    (if (eof-object? *remote-word-list*) (display "Error: eof-object")
        (interpet *remote-word-list*))
    (loop))
  (loop))
  
(define (send-thread)
  (define (loop)
    (send-string (make-message *object-list*))
    (loop))
  (loop))

(define (make-message lst)
  (let ((str ""))
    (define (msg-loop iter-lst)
      (cond
        ((null? iter-lst) (substring str 1))
        ((is-a? (car iter-lst) snowball%)
         (set! str (string-append str " make-snowball "  
                                  (number->string (send (car iter-lst) get-x)) " "
                                  (number->string (send (car iter-lst) get-y))))
         (msg-loop (cdr iter-lst)))
        ((is-a? (car iter-lst) player%)
         (set! str (string-append str 
                                  " make-player " 
                                  (number->string (send (car iter-lst) get-x)) " "
                                  (number->string (send (car iter-lst) get-y))))
         (msg-loop (cdr iter-lst)))))
   (msg-loop lst)))
 
(define (send-string string)
  (display string *outport*)
  (newline *outport*)
  (display "" *outport*))

;----------------interpeting of messages--------------------

(define (interpet str)
  (set! *remote-word-list* (string->wordlist str))
  (update-remote-objectlist *remote-word-list*))

(define (update-remote-objectlist lst)
  (let ((temp-object-list '()))
    (define (new-temp remote-lst)
      (if (null? remote-lst) (void)
          (begin (set! temp-object-list 
                       (cons (apply (get 'remote-commands (string->symbol (car remote-lst))) 
                                    (cons (cadr remote-lst) (cons (caddr remote-lst) '())))
                             temp-object-list))
                 (new-temp (cdddr remote-lst)))))
  (new-temp lst)
  (semaphore-wait *rol-semaphore*)
  (set! *remote-object-list* temp-object-list)
  (semaphore-post *rol-semaphore*)))

(define (first-word charlist)
      (if (or (null? charlist) (equal? (car charlist) #\space))
          ""
          (string-append (string (car charlist)) (first-word (cdr charlist)))))

(define (string->wordlist string) ;not fully generalised
  (let ((current-word ""))
    (define (st->w str)
      (begin
        (set! current-word (first-word (string->list str)))
        (cond
          ((= (string-length current-word) (string-length str)) (cons  current-word '()))
          ((equal? current-word "") '())
          (else (cons current-word (st->w (substring str (+ 1 (string-length current-word)))))))))
    (st->w string)))

;-------------------command-table init---------------------
(define (remote-make-snowball . args)
  (new snowball% 
        [sprite (make-object bitmap% "snowball.png" 'png/alpha #f)]
        [x (string->number (car args))]
        [y (string->number (cadr args))]))

(define (remote-make-player . args)
  (new player% 
        [sprite (make-object bitmap% "testbild.png" 'png/alpha #f)]
        [x (string->number (car args))]
        [y (string->number (cadr args))]))

(put 'remote-commands 'make-snowball remote-make-snowball)
(put 'remote-commands 'make-player remote-make-player)


;------------------interface------------------

(define (get-remote-objects)
  *remote-object-list*)
  
(define (start-send lst)
  (thread send-thread))
