(load "/Users/jonathan/Documents/SourceCode/extempore_maschine/startup.scm")

(define audio-files (vector
   "/Users/Shared/Maschine Library/Samples/One Shots/Strike/Bend ClickStart.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Glitch/Crup Swag.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Acoustic Note/Purehigh D6.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Metal/Metallic Digigong 1.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Noise/Noise ProgHouse.wav"
   "/Users/Shared/Maschine Library/Samples/Instruments/Bass/Cute Monster Samples/CuteMonster f#3.wav"
   "/Users/Shared/Maschine Library/Samples/Instruments/Pad/Dweller Samples/Dweller c5.wav"
   "/Users/Shared/Maschine Library/Samples/Instruments/Mallet/Sansa Samples/Sansa C3.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist BitBreakup.wav"
   "/Users/Shared/Maschine Library/Samples/One Shots/Distortion/Dist CircuitBent 1.wav"))

(define *audio-data* (make-initialized-vector (vector-length audio-files) (lambda (i)
   (au:load-audio-data (vector-ref audio-files i)))))

(define mixer (au:make-node "aumx" "smxr" "appl"))
(au:connect-node mixer 0 *au:output-node* 0)
(au:update-graph)

(define samplers (make-initialized-vector (vector-length *audio-data*) (lambda (i)
   (let ((sampler (au:make-node "aumu" "play" "MOSO")))
      (au:connect-node sampler 0 mixer i)
      (au:update-graph)
      (au:play:set-sample-data sampler 64 (vector-ref *audio-data* i))
      sampler))))

(au:start-graph)

(define (spin i) (let loop ((j 0))
   (if (not (= i j))
      (begin
         (loop (+ j 1))))))