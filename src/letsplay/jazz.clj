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

;; swing
(defn offnote? [time]
  (= (mod time 1 ) 0.5))

(defn groove [time]
  (if (offnote? time)
    (+ time 0.2)
    time))

(defn play-bar [m bar-beat bar]
  (doseq [hit ((deref bar))]
    (let [hit-time (groove (first hit))
          instr (second hit)]
      (at (m (+ bar-beat hit-time))
        (instr)))))


(defn loop-play [m bar len]
  (let [beat (m)]
    (play-bar m beat bar)
    (apply-at (m (+ len beat)) #'loop-play [m bar len])))

(def length 4)

(defn jazzdrums
  []
  ;; filter out all nils
  (filter #(not (nil? %))
          (concat
           ;; ride on every beat
           (map (fn [t] [t ride]) (range 0 length))
           ;; off-beat ride
           (map #(when (< (rand) 0.3) [% ride]) (range 0.5 length))

           ;; snaps on every other beat
           ;; the snaps are a bit late, subtract a bit to get them on time
           (map (fn [t] [(- t 0.02) snap]) (range 1 length 2))

           ;; off-beat snare once in a while
           (map #(when (< (rand) 0.1) [% snare]) (range 0.5 length))

           ;; 'hit' consisting of cymbal+kick at some random off-beat
           ;; doing it this way allows us to place two drums on same beat
           (when (< (rand) 0.1)
             (let [t (+ 0.5 (rand-int length))]
               (list [t kick] [t cymbal])))
           )))

(defn limit [n minimum maximum]
  (max minimum
       (min maximum n)))

(defn jazzbass [m n]
  (let [beat (m)
        tick (m beat)
        note (if (not (zero? (mod beat 2)))
               (dec n)
               ;; keep tone inside interval
               ;; TODO - avoid hanging around at the limits
               (limit (+ n (rand-nth '(-7 -6 -5 5 6 7))) 40 65))]
    (at tick
      (+ (beep note) (bass (midi->hz note))))
    ;; extra off-beat note with same tone
    (when (> 0.1 (rand))
      (at (m (+ beat (groove 0.5)) )
         (+ (beep note) (bass (midi->hz note)))))
    (apply-at (m (+ beat 1)) #'jazzbass [m note])))


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
;; (jazzbass metro 45)

;; (stop)

;; TODO - a way of ensuring that we start drums+bass at (zero? (mod beat 4))


;; TODO - some way to go to double tempo - the one below turns music into noise!
;; (metro :bpm (* 2 tempo))

;; And back to music!
;; (metro :bpm tempo)
