(ns letsplay.jazz
  (:use [overtone.live]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [letsplay.rotater]))

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
(defn offnote? [time]
  (= (mod time 1) 0.5))

;; todo - swing
(defn swing [time]
  (if (offnote? time)
    (+ time 0.2)
    time))

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
           (map (fn [t] [t ride]) (range 0 4))

           ;; off-beat ride
           (map #(when (weighted-coin 0.3) [% ride]) (range 0.5 4))

           ;; snaps on every other beat
           (map (fn [t] [(+ 0.02 t) snap]) (range 1 4 2))

           ;; the snaps are a bit late, subtract a bit to get them on time


           ;; off-beat snare once in a while
           (map #(when (weighted-coin 0.1) [% snare]) (range 0.5 4))

           ;; 'hit' consisting of cymbal+kick at some random off-beat
           ;; doing it this way allows us to place two drums on same beat
;;           (when (weighted-coin 0.3) (let [b (rand-nth (range 0.5 4))] (list [b kick] [b cymbal])))
           )))

(defn limit [n minimum maximum]
  (max minimum
       (min maximum n)))

(defn jazzbass [n]
  (let [beat (metro)
        tick (metro beat)
        note (if (not (zero? (mod beat 2)))
               (dec n)
               ;; keep tone inside interval
               ;; TODO - avoid hanging around at the limits
               (limit (+ n (rand-nth '(-7 -6 -5 5 6 7))) 40 65))]
    (at tick
      (beep note)
      (bass (midi->hz note)))
    ;; extra off-beat note with same tone
    (when (> 0.1 (rand))
      (at (metro (+ beat (swing 0.5)) )
        (beep note)
        (bass (midi->hz note))))
    (apply-at (metro (+ beat 1)) #'jazzbass [note])))

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
;; (loop-play metro #'jazzdrums length)

;; Play bass
;; (jazzbass 45)

;; (stop)

;; TODO - a way of ensuring that we start drums+bass at (zero? (mod beat 4))


;; TODO - some way to go to double tempo - the one below turns music into noise!
;; (metro :bpm (* 0.5 tempo))

;; And back to music!
;; (metro :bpm tempo)
