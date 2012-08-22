(ns letsplay.jazz
  (:use [overtone.live]
        [overtone.inst.drum]
        [overtone.inst.synth]
        [letsplay.rotater]))

(remove-handler :breakbeat-handler)

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

(def jazz-intervals '(-7 -6 -5 5 6 7))
(def maxbass 40)
(def minbass 65)

;; todo - jazzbass
(defn jazzbass [n]
  (let [beat (metro)
        tick (metro beat)
        note (if (not (zero? (mod beat 2)))
               ;; just go half a step down
               (dec n)
               ;; keep tone inside interval
               ;; TODO - avoid hanging around at the limits
               (limit (+ n (rand-nth jazz-intervals)) maxbass minbass))]
    (at tick
      (beep note)
      (bass (midi->hz note)))
    ;; extra off-beat note with same tone
    (when (> 0.1 (rand))
      (at (metro (+ beat (swing 0.5)) )
        (beep note)
        (bass (midi->hz note))))
    (apply-at (metro (+ beat 1)) #'jazzbass [note])))


;; Place cursor at the end of these expressions
;; and do C-x e to execute them

;; Set up rotater

(def device-filter [ :midi-device "Novation DMS Ltd" "Launchpad" "Launchpad"])


(on-event (conj device-filter :note-on)
          (fn [e]
            (rotater e 0))
          :handle-rotate-on)

(on-event (conj device-filter :note-off)
          (fn [e]
            (rotater e 0))
          :handle-rotate-off)

;; TODO - set up midi output device from this file!

;; Play drums
;; (loop-play #'jazzdrums length)

;; Play bass
;; (jazzbass 45)

;; (stop)

;; TODO - a way of ensuring that we start drums+bass at (zero? (mod beat 4))


;; TODO - some way to go to double tempo - the one below turns music into noise!
;; (metro :bpm (* 2 tempo))

;; And back to music!
;; (metro :bpm tempo)
