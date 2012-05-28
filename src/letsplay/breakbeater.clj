(ns letsplay
  (:use overtone.live))

(def loop-playing (ref {}))

;; outputs a buffer to use with play-buf
(defn freebb [n]
  (let [result (freesound-search "breakbeat")
        id (:id (nth result n))]
    (load-sample (freesound-path id))))

(defn loga [a n]
  (/ (Math/log n) (Math/log a)))

(defn smart-rate [sample wanted-dur]
  (let [dur (:duration sample)
        pre-r  (/ dur  wanted-dur)
        exp (Math/ceil (loga 2 pre-r))]
    (let [rate (/ pre-r (Math/pow 2 exp))]
      (if (< rate 0.8)
        (* 2 rate)
        rate))))

;; playbuf: channels sample-buf rate trigger vol start loop
(defn bbsynth [sample rate]
  (let [channels 1
        trigger-vol 1
        start 0
        loop 1]
    (inst (play-buf channels sample rate trigger-vol start loop))))

(def wanted-dur 1.5)

(defn startloop [n time]
  (let [sample (freebb n)
        rate (smart-rate sample wanted-dur)
        synth (bbsynth sample rate)
        dur (* 1000 (/ 1 rate) (:duration sample))]
    (at time
      (kill @loop-playing)
      (dosync
       (ref-set loop-playing synth)
       (loop-playing)))))


(def tempo 160)

(def bps (/ tempo 60))

(def wanted-dur (* 4 (/ 1 bps)))

(def metro (metronome tempo))

(defn nextbar []
  (let [beat (metro)
        bar-remaining (mod beat 4)]
    (+ beat (- 4 bar-remaining))))

(def launchpad (midi-in "Launchpad"))

(defn break-it [event ts]
  (let [cmd (:cmd event)
        note (:note event)]
    (if (= cmd :note-on)
      (startloop note (metro (nextbar))))))

(midi-handle-events launchpad #'break-it)
