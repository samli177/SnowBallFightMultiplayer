(load "table-helper.scm")

(define network-session%
  (class object%
    (super-new)
    (init-field (host "192.168.1.104")
                (port 9000))
    (field (inport null)
           (outport null)
           
           (remote-word-list '())
           (remote-object-list '())
           (change-check '())
           
           (rol-semaphore (make-semaphore 1))
           (sync #t))
    ;--------actual networking-stuff-------
    
    
    (define (listen-for-data)
      (define (loop)
        (set! remote-word-list (string->wordlist (read-line inport 'any)))
        (if (eof-object? remote-word-list) (display "Error: eof-object"))
        (if (eq? (string->symbol (car remote-word-list)) 'sync) (set! sync #t)
            (begin (interpet remote-word-list) (send-string "sync")))
        (loop))
      (loop))
    
    (define/public (start-send)
      (thread send-thread))
    
    (define (send-thread)
      (define (loop)
        (if (and (not (eq? change-check *object-list*)) sync)
            (begin (send-string (make-message *object-list*)) (set! change-check *object-list*) (set! sync #f)))
        (sleep .01)
        (loop))
      (set! change-check *object-list*)
      (loop))
    
    
    
    ;----------------interpeting and construction of messages--------------------
    
    (define/public (make-message lst) ; Constructs a message string from list of objects
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
             (msg-loop (cdr iter-lst)))
            ((is-a? (car iter-lst) bunker%)
             (set! str (string-append str 
                                      " make-bunker " 
                                      (number->string (send (car iter-lst) get-x)) " "
                                      (number->string (send (car iter-lst) get-y))))
             (msg-loop (cdr iter-lst)))
            (else (msg-loop (cdr iter-lst)))))
        (msg-loop lst)))
    
    (define (interpet wordlst)
      (cond 
        ((eq? (string->symbol (car wordlst)) 'hit) (hit-player!))
        (else (update-remote-objectlist wordlst))))
    
    (define (hit-player!)
      (for-each (lambda (object) (if (is-a? object player%) (send object hit!))) *object-list*))
    
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
        (semaphore-wait rol-semaphore)
        (set! remote-object-list temp-object-list)
        (semaphore-post rol-semaphore)))
    
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
           [radius (/ (send (make-object bitmap% "snowball.png" 'png/alpha #f) get-width) 2)]
           [x (string->number (car args))]
           [y (string->number (cadr args))]))
    
    (define (remote-make-player . args)
      (new player% 
           [sprite (make-object bitmap% "blagubbe.png" 'png/alpha #f)]
           [radius (/ (send (make-object bitmap% "blagubbe.png" 'png/alpha #f) get-width) 2)]
           [x (string->number (car args))]
           [y (string->number (cadr args))]))
    
    (define (remote-make-bunker . args)
      (new player% 
           [sprite (make-object bitmap% "bunker.png" 'png/alpha #f)]
           [radius (/ (send (make-object bitmap% "bunker.png" 'png/alpha #f) get-width) 2)]
           [x (string->number (car args))]
           [y (string->number (cadr args))]))
    
    (put 'remote-commands 'make-snowball remote-make-snowball)
    (put 'remote-commands 'make-player remote-make-player)
    (put 'remote-commands 'make-player remote-make-player)
    
    ;---------------------interface--------------------------
    
    ;---------------------get-methods------------------------
    
    (define/public (get-host) host)
    
    (define/public (get-remote-objects) remote-object-list)
    
    ;---------------------set-methods------------------------
    
    (define/public (set-host! new-host) (set! host new-host))
    
    ;---------------------actions---------------------------
    
    
    
    (define/public (listen)
      (let ((listener (tcp-listen port 1 #t)))
        (set!-values (inport outport) (tcp-accept listener))
        (thread listen-for-data)
        (start-send)))
    
    (define/public (send-string string)
      (display string outport)
      (newline outport)
      (display "" outport))
    
    (define/public (hit!)
      (send-string "hit"))
    
    (define/public (connect) 
      (set!-values (inport outport) (tcp-connect host port))
      (thread listen-for-data)
      (start-send))))
    