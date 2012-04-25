(ns betafunk.jazz
  (:use overtone.live))

;; a bit of pattern matching
(use '[clojure.core.match :only [match]])

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
