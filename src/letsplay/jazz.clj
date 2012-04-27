(ns betafunk.jazz
  (:use [overtone.live] [overtone.inst.drum] [overtone.inst.synth]))

;; a bit of pattern matching
(use '[clojure.core.match :only [match]])

(definst beep [note 60]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.9) :action FREE)]
    (* src env)))

(beep 45)
;; Specify input device
;; No arg will list midi devices in pop-up and
;; allow you to select one
(def kb (midi-in "FastTrack Pro"))

;; Specify output device
(def synth-out (midi-out "FastTrack Pro"))

;; Rotate between these notes
(def rotate (ref '(-10 -7 -14 -5)) )

(defn next-rotate []
  (let [note (first @rotate)]
    (ref-set rotate (concat (rest @rotate) (list note)))
    note))

;; Init a vector of 128 empty lists
(def notes-playing
  (ref (vec (repeat 128 '()))))

(defn add-notes [note notes]
  (ref-set notes-playing
           (assoc @notes-playing note notes)))

(defn expander [event ts]
  (let [chan (:chan event)
        cmd (:cmd event)
        note (:note event)]
    (match [chan cmd]
      [_ 144] (dosync ;; (next-rotate) and (add-notes) must be sync'ed
               (let [notes (map #(+ % note) [(next-rotate) 0 7])]
                 (add-notes note notes) ;; mapping note => notes
                 (doseq [n notes] (midi-note-on synth-out n (:vel event)))))
      [_ 128] (let [notes (@notes-playing note)]
                (doseq [n notes] (midi-note-off synth-out n))))))

;; the expander function to handle incoming midi
;; (midi-handle-events kb #'expander)

;; ride cymbal
(def ride (sample (freesound-path 436)))
(ride)

(def metro (metronome 160))

(defn offnote? [time]
  (= (mod time 1 ) 0.5))

(defn groove [time]
  (if (offnote? time)
    (+ time 0.2)
    time))

(defn my-play [bar]
  (doseq [hit (deref bar)]
    (let [time (groove (first hit))
          instr (second hit)]
      (at (metro (+ time (metro)))
        (instr)))))


(defn loop-play [bar len]
  (my-play bar)
  (apply-at (metro (+ len (metro))) #'loop-play [bar len]))


;; re-evaluate to improvise some ride!

(def length 8)

(def jazzdrums
  (filter #(not (nil? %))
          (concat (map (fn [t] (when (< (rand) 0.3) [t ride])) (range 0.5 length))
                  (map (fn [t] [t ride]) (range 0 length))
                  (map (fn [t] [t c-hat]) (range 1 length 2))
                  (map (fn [t] (when (< (rand) 0.1) [t snare])) (range 0.5 length))
                  )))

;; (loop-play #'jazzdrums length)

(defn jazzbass [n]
  (let [beat (metro)
        note (if (zero? (mod beat 2))
               (dec n)
               (max 40
                    (min 65
                         (+ n (rand-nth '(-7 -6 -5 5 6 7))))))]
    (beep note)
    (apply-at (metro (+ (metro) 1)) #'jazzbass [note])))


;; (jazzbass 45)

;; (stop)
