;; table-helper.scm

;; New version for Scheme standard 6, R6RS

;; This file provides the following procedures: 
;; put, get, display-table.
;:
;; load it by adding the following expression
;; to the beginning of your .scm-file:
;; (load "table-helper.scm")

;; Note to students: you do not need to understand this code
;; when you do lab02a, however, the techniques used below
;; will be introduced in lab03-05


(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (lookup key-1 key-2)
      (let ((subtable (m-assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (m-assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (m-assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (m-assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key-1
                                     (mcons (mcons key-2 value) '()))
                              (mcdr local-table)))))
      'ok)
    
    (define (display* . args)
      (for-each display args))
    
    (define (display-table)
      (m-for-each (lambda (subtable)
                    (m-for-each (lambda (entry)
                                  (display* (mcar subtable)
                                            "\t  \t"
                                            (mcar entry)
                                            "\t  \t"
                                            (mcdr entry)
                                            "\n"))
                                (mcdr subtable)))
                  (mcdr local-table)))
    
    (define (m-assoc key l)
      (cond ((null? l) #f)
            ((equal? key (mcar (mcar l))) (mcar l))
            (else (m-assoc key (mcdr l))))) 
    
    (define (m-for-each fn l)
      (if (null? l) 
          '() 
          (begin (fn (mcar l)) (m-for-each fn (mcdr l)))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-table) display-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define display-table (operation-table 'display-table))
