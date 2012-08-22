(ns letsplay.jam
  (:use [overtone.live]
        [overtone.inst.drum]))

;; this loads a sample into a buffer
(defn fs-buffer [q n]
  (let [result (freesound-search :q q :f "duration:[0.0 TO 1.0]")
        id (:id (nth result n))]
    (load-sample (freesound-path id))))

;; this makes a quick play buffer with a freesound in it!
(defn quickfree
  ([query n] (quickfree query n 1))
  ([query n rate]
     (play-buf 1 (fs-buffer query n) rate)))

;; random noises from freesound
(definst noise1 [vol 1] (* vol (quickfree "ding" 3)))

(definst noise2 [vol 1]  (* vol (quickfree "snap" 7 3)))

(definst noise3 [vol 1] (* vol (quickfree "cluck" 1)))

(definst noise4 [vol 1] (* vol (quickfree "pling" 6 3)))

;; todo - more noices

(def noises (list noise1 noise2 noise3 noise4))

;; initial beep
(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

;; todo - more interesting beep
;; crossfade, more oscs, detune
(definst beep [note 60 vol 1]

  )

(def metro (metronome 160))

(defn my-play
  ([tones] (let [next-beat (metro)]
             (my-play next-beat tones)))
  ([beat tones]
     (let [tone (first tones)
           time (metro beat)
           divider 2]
       (at time
         (beep (:note tone) (:vol tone)))
       (let [next-beat (+ (/ 1.0 divider) beat)]
         (apply-at (metro next-beat) my-play next-beat (rest tones) [])))))

(def rhythm (flatten (repeat 4 '(1 0 1 0 1 0 1 0))))

(def tones (flatten (map #(map (fn [p] (+ p (* 12 %)))
                               (degrees->pitches '(:i :iii :iv :v :vii) :aeolian :D3 )) (range 2))))

(defn melody
  ([rhythm tones] (melody [] rhythm tones 0 (fn [n] 1)))
  ([rhythm tones accfn] (melody [] rhythm tones 0 accfn))
  ([result rhythm tones n accfn]
     (if (or (nil? (first rhythm))
             (nil? (first tones)))
       result
       (let [rhythm-tail (rest rhythm)
             vol (first rhythm)
             tone (if (zero? vol)
                    {:note (- (first tones) 12)
                     :vol 0.6}
                    {:note (first tones)
                     :vol (* vol (accfn n))})
             tones-tail (if (> (first rhythm) 0)
                          (rest tones)
                          tones)]
         (recur (conj result tone) rhythm-tail tones-tail (inc n) accfn)))))

;; melody accents
(defn mel-acc [n]
  (if (zero? (mod n 3)) 1.0 0.1))

;; drum accents
(defn drum-acc [n]
  (let [m (mod n 4)]
    (nth '(0.5 0.2 0.8 0.2) m)))

;; todo - drumbeat
(defn drumbeat
  ([beat] (drumbeat beat 0))
  ([beat n]
;; ...
     ))

(def tones '(65 64 60))

(def rhythm '(1 1 1))

;; (my-play (flatten (repeatedly (fn [] (melody rhythm tones mel-acc)))))

;; (drumbeat (metro))
