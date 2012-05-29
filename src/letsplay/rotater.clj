;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Midi rotater                ;;
;;                             ;;
;; Inspired by this video:     ;;
;; http://youtu.be/4kBpxBJkknY ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns letsplay.rotater
  (:use [overtone.live]))

;; a bit of pattern matching
(use '[clojure.core.match :only [match]])


;; Specify output device
(def synth-out (midi-out "FastTrack Pro"))

;; Rotate between these notes
(def rotate (ref '(-10 -7 -14 -5)) )

;; TODO - use a pointer into the list mod list length instead
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

;; the rotater function to handle incoming midi
(defn rotater [event ts]

  (let [chan (:chan event)
        cmd (:cmd event)
        note (:note event)]
    (match [chan cmd]
      ;; note-on
      [_ :note-on] (dosync ;; (next-rotate) and (add-notes) must be sync'ed
               (let [notes (map #(+ % note) [(next-rotate) 0 7])]
                 (prn "out")
                 (add-notes note notes) ;; mapping note => notes
                 (doseq [n notes] (midi-note-on synth-out n (:vel event)))))
      ;; note-off
      [_ :note-off] (let [notes (@notes-playing note)]

                (doseq [n notes] (midi-note-off synth-out n))))))
