(define-syntax define-simple-syntax
  (syntax-rules ()
     ((_ (name arg ...) body ...)
      (define-syntax name (syntax-rules () ((name arg ...) (begin body ...)))))))

(define (list-index s los comp)
   (cond ((null? los) -1)
         ((comp s (car los)) 0)
         (else
            (let ((res (list-index s (cdr los) comp)))
               (if (= res -1)
                  -1
                  (+ res 1))))))

(define (intlist-index s los)
   (list-index s los =))

(define (strlist-index s los)
   (list-index s los string=?))

(define (set-list-ref los n v)
   (cond ((null? los) #f)
         ((< n 0) #f)
         ((= n 0) (set-car! los v))
         (else (set-list-ref (cdr los) (- n 1) v))))

(define srcMaschine (io:midi-source 0))
(define srcMidiSportA (io:midi-source 1))
(define srcMidiSportB (io:midi-source 2))

(define-simple-syntax (list-mixers)
   (au:print-audiounits "aumx"))
(define-simple-syntax (list-generators)
   (au:print-audiounits "augn"))
(define-simple-syntax (list-instruments)
   (au:print-audiounits "aumu"))

(define (make-initialized-vector length initialization)
   ;; LENGTH is checked by MAKE-VECTOR
   (let ((vector (make-vector length)))
      (let loop ((index 0))
         (if (< index length)
             (begin
                (vector-set! vector index (initialization index))
                (loop (+ index 1)))))
    vector))
