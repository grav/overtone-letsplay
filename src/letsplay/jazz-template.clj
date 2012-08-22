(ns letsplay.jazz
  (:use [overtone.live]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [letsplay.rotater]))

(remove-all-handlers)

;; just a simple example of a synth
;; we'll use this together with the bass
(definst beep [note 60]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.9) :action FREE)]
    (* src env)))

;; drums
(def ride (sample (freesound-path 436)))

(def cymbal (sample (freesound-path 13254)))

(def snap (sample (freesound-path 87731)))

;; todo - offnote


;; todo - swing


(def tempo 160)

(def metro (metronome tempo))

(defn play-bar [bar-beat bar]
  (doseq [hit (bar)]
    (let [hit-time (swing (first hit))
          instr (second hit)]
      (at (metro (+ bar-beat hit-time))
        (instr)))))

(defn loop-play [bar len]
  (let [beat (metro)]
    (play-bar beat bar)
    (apply-at (metro (+ len beat)) #'loop-play [bar len])))

(def length 4)

(stop)


;; todo - drum pattern
;; list of pairs (beat, instrument)
(defn jazzdrums
  []
  ;; filter out all nils
  (filter #(not (nil? %))
          (concat
           ;; ride on every beat


           ;; off-beat ride


           ;; snaps on every other beat


           ;; the snaps are a bit late, subtract a bit to get them on time


           ;; off-beat snare once in a while


           ;; 'hit' consisting of cymbal+kick at some random off-beat
           ;; doing it this way allows us to place two drums on same beat

           )))

(defn limit [n minimum maximum]
  (max minimum
       (min maximum n)))

(def jazz-intervals '(-7 -6 -5 5 6 7))
(def maxbass 40)
(def minbass 65)

;; todo - jazzbass taking start note


(stop)
;; tempo
(def tempo 160)
(def metro (metronome tempo))

;; Specify input device
;; No arg will list midi devices in pop-up and
;; allow you to select one
(def kb (midi-in "FastTrack Pro"))

;; Place cursor at the end of these expressions
;; and do C-x e to execute them

;; Set up rotater
;; (midi-handle-events kb #'rotater)

;; TODO - set up midi output device from this file!

;; Play drums
;; (loop-play #'jazzdrums length)

;; Play bass
;; (jazzbass 45)

;; (stop)

;; TODO - a way of ensuring that we start drums+bass at (zero? (mod beat 4))


;; TODO - some way to go to double tempo - the one below turns music into noise!
;; (metro :bpm (* 0.5 tempo))

;; And back to music!
;; (metro :bpm tempo)
