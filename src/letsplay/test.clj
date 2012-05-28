(ns letsplay
  (:use [overtone.live]
        [overtone.inst.drum]))

(defn fs-buffer [q n]
  (let [result (freesound-search :q q :f "duration:[0.0 TO 1.0]")
        id (:id (nth result n))]
    (load-sample (freesound-path id))))

;; this makes a quick buffer with a freesound in it!
(defn quickfree
  ([query n] (quickfree query n 1))
  ([query n rate]
     (play-buf 1 (fs-buffer query n) rate)))

;; TODO - this should be more dynamic
(definst noise1 [vol 1] (* vol (quickfree "ding" 3)))

(definst noise2 [vol 1]  (* vol (quickfree "snap" 7 3)))

(definst noise3 [vol 1] (* vol (quickfree "cluck" 1)))

(definst noise4 [vol 1] (* vol (quickfree "pling" 6 3)))

(def noises (list noise1 noise2 noise3 noise4))

(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

(definst beep [note 60 vol 1]
  (let
      [crossfade 0.7
       freq (midicps note)
       detune (* 0.005 freq (sin-osc 10))
       src1 (sin-osc (- freq detune))
       src2 (sin-osc (+ freq detune))
       src3 (saw (- freq detune))
       src4 (saw (+ freq detune))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* (+
        (* (- 1 crossfade) (+ src1 src2))
        (* crossfade (+ src3 src4)))
       env (* 0.5 vol)) ))

(def metro (metronome 160))

(metro (metro))

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

(def rhythm  (flatten (repeat 4 '(1 0 1 0 1 0 1 0))))

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
                    {:note (- (first tones) 7)
                     :vol 0.6}
                    {:note (first tones)
                     :vol (* vol (accfn n))})
             tones-tail (if (> (first rhythm) 0)
                          (rest tones)
                          tones)]
         (recur (conj result tone) rhythm-tail tones-tail (inc n) accfn)))))

;;(my-play (melody rhythm tones (fn [n] (if (zero? (mod n 4)) 1 0.5))))

(defn accfn [n]
  (if (zero? (mod n 3)) 1.0 0.1))

;;(my-play (flatten (repeatedly (fn [] (melody rhythm tones accfn)))))


(defn acntfn [n]
  (let [m (mod n 4)]
    (nth '(0.5 0.2 0.8 0.2) m)))

(defn drumbeat
  ([beat] (drumbeat beat 0))
  ([beat n]
     (let [time (metro beat)
           next-beat (+ 0.5 beat)]
       (if (zero? (mod n 4))
         (at time
           (kick)))
       (if (weighted-coin 0.8)
         (at time
          ((rand-nth noises) (acntfn n))))

       (apply-at (metro next-beat) #'drumbeat next-beat (inc n) []))))

(stop)

;; (drumbeat (metro))
