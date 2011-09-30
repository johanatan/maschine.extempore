(load "/Users/jonathan/Documents/SourceCode/extempore_maschine/audio-data.scm")

(define src srcMaschine)
;; (define src srcMidiSportA)
;; (define src srcMidiSportB)

(define dials (make-vector (vector-length samplers)
      (list
         (list "pitch" "volume" "chop" "attack" "decay" "sustain" "release") ;; parameter
         (list  7       7        6      6        6       6         6)        ;; channel
         (list  1       2        3      4        5       6         7)        ;; dial
         (list  60      100      0      64       64      64        64))))    ;; value

(define (set-dial-by-index dial-set i val)
   (set-list-ref (car (cdddr dial-set)) i val))

(define (set-dial-by-id dial-set id val)
   (let ((i (intlist-index id (caddr dial-set))))
      (set-dial-by-index dial-set i val)))

(define (get-dial-index dial-set dial)
   (strlist-index dial (car dial-set)))

(define (set-dial-by-str dial-set dial value)
   (set-dial-by-index (get-dial-index dial-set dial) value))

(define (get-dial-val dial-set dial)
   (list-ref (car (cdddr dial-set)) (get-dial-index dial-set dial)))

(define-simple-syntax (adjust param value)
   (set! param value))

(define (clip val minimum maximum)
   (max (min val maximum) minimum))

(define-simple-syntax (toggle param value)
   (set! param (if (> value 0) #t #f)))                   

(define (play-sample-vol index volume)
   (play-note (now) (vector-ref samplers index)
              (get-dial-val (vector-ref dials index) "pitch")
              (clip 0 127 volume)
              (* 8000 (get-dial-val (vector-ref dials index) "decay"))))

(define (play-sample index)
   (play-sample-vol index (get-dial-val (vector-ref dials index) "volume")))

(define cur-pad 0)
(define (handle-dial channel dial value)
   (cond
      ((and (>= dial 1) (<= dial 7))
         (set-dial-by-id (vector-ref dials cur-pad) dial value)
         (print (vector-ref dials cur-pad))
         (play-sample cur-pad))))

(define pad-translation (vector 12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3))
(define (translate-pad pad)
   (vector-ref pad-translation pad))

(define note-repeat #f)
(define (handle-pad channel pad velocity)
   (print pad)
   (cond
      ((and (<= pad 15) (>= pad 0))
         (set! cur-pad (translate-pad pad))
         (print (vector-ref dials cur-pad))
         (if (> velocity 0)
            (begin
               (play-sample-vol cur-pad (+ 35 velocity))
               (print note-repeat)
               (if note-repeat
                  (callback (+ (now) 5000) 'handle-pad channel pad velocity)))))
      ((= pad 127) (toggle note-repeat velocity))))

(define-simple-syntax (midi-log)
   (define io:midi-in
      (lambda (device type channel a b)
         (print device type channel a b)
         (print src device))))

(define-simple-syntax (midi-play)
   (define io:midi-in
      (lambda (device type channel a b)
         (cond
            ((= type 11) (handle-dial channel a b))
            ((= type 9) (handle-pad channel a b))))))

(midi-log)
(midi-play)

(play-note (now) (vector-ref samplers 0) 60 127 185000)
