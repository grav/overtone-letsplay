(ns letsplay
  (:use overtone.live))

(definst beep [note 60]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

;; one way of enabling dynamic binding
(defn my-play [time tones sep]
  (let [tone (first tones)]
    (at time
        (beep (+ 0 tone)))
    (let [next-time (+ time sep)]
      (apply-at next-time my-play next-time (rest tones) sep []))))

(def tones)

(defn loop-play [time tones sep]
  (my-play time (flatten (repeatedly (fn [] (deref #'tones)))) 200))

;; (def tones [65 69 72])
;; (loop-play (now) tones 200)
;; redefining tones will work, ie
;; (def tones [67 66 65])

(defn updown [tones]
  (concat tones  (reverse (drop 1 (take (dec (count tones)) tones)))))

(def tones
  (degrees->pitches [:i :iii :v :vii] :ionian :F3))

(freesound-path 52280)
 (defn tree-play [time tree sep]
  (let [node (first tree)]
    (when node
      (println node)
      (if (not (list? node))
        (at time
            (beep node))
        (dotimes [n (count node)]
          (tree-play [])
          )))
    (let [next-time (+ time sep)]
      (apply-at next-time
                tree-play [next-time (rest tree) sep]))))

;;(ctl my-play :sep 200)
(stop)

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
    (/ pre-r (Math/pow 2 exp))))

(smart-rate (freebb 0) 1.5)

(defn bbsynth [sample rate]
  (synth
   (let
       [buf (play-buf 1 sample rate)]
     (out 0 [buf buf]))))

(bbsynth (freebb 0) 1)

(def wanted-dur 1.5)

(defn playsolo [synth]
  (dosync
   (stop)
   (synth)))


(defn loopplay [n time]
  (let [sample (freebb n)
        rate (smart-rate sample wanted-dur)
        synth (bbsynth sample rate)
        dur (* 1000 (/ 1 rate) (:duration sample))]
    (at time
      (stop)
      (synth))
    (let [next (+ time dur)]
      (apply-at next #'loopplay [n next] ))))


(def tempo 160)

(def bps (/ tempo 60))

(def wanted-dur (* 4 (/ 1 bps)))

(def metro (metronome tempo))

(defn nextbar []
  (let [beat (metro)
        bar-remaining (mod beat 4)]
    (+ beat (- 4 bar-remaining))))

(loopplay 0 (metro (nextbar)))
(stop)

(def r 3.95)

(/ (Math/floor r) 1)
