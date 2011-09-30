(load "/Users/jonathan/Documents/SourceCode/extempore_maschine/audio-data.scm")

(define src srcMaschine)
;; (define src srcMidiSportA)
;; (define src srcMidiSportB)

(define dials (make-vector (vector-length samplers)
      (list
         (list "pitch" "volume" "pace" "duration" "attack" "decay" "sustain" "release") ;; parameter
         (list  7       7        7      6          6        6       6         6)        ;; channel
         (list  11      12       13     14         15       16      17        18)       ;; dial
         (list  60      100      64     64         64       64      64        64))))    ;; value

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

(define (clip minimum maximum val)
   (max (min val maximum) minimum))

(define (scale minimum maximum val)
   (truncate (+ minimum (* (/ val 127) maximum))))

(define-simple-syntax (toggle param value)
   (set! param (> value 0)))                   

(define (get-param-val index parameter)
   (get-dial-val (vector-ref dials index) parameter))

(define (play-sample-vol index volume)
   (play-note (now) (vector-ref samplers index)
              (get-param-val index "pitch")
              (clip 0 127 volume)
              (* 8000 (get-param-val index "duration"))))

(define (play-sample index)
   (play-sample-vol index (get-param-val index "volume")))

(define cur-pad 0)
(define (handle-dial channel dial value)
   (cond
      ((and (>= dial 11) (<= dial 17))
         (set-dial-by-id (vector-ref dials cur-pad) dial value)
         (print (vector-ref dials cur-pad)))))

(define pad-translation (vector 12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3))
(define (translate-pad pad)
   (vector-ref pad-translation pad))

(define (get-pace index)
   (scale 250 7500 (get-param-val cur-pad "pace")))

(define note-repeat #f)
(define (handle-pad channel pad velocity)
   (cond
      ((and (<= pad 15) (>= pad 0))
         (set! cur-pad (translate-pad pad))
         (print (vector-ref dials cur-pad))
         (if (> velocity 0)
            (begin
               (play-sample-vol cur-pad (min (+ 35 velocity) (get-param-val cur-pad "volume")))
               (if note-repeat
                  (callback (+ (now) (get-pace cur-pad)) 'handle-pad channel pad velocity)))))
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