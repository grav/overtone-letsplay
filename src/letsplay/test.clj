(ns letsplay
  (:use [overtone.live]))

(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

(definst beep [note 60 vol 1]
  (let
      [freq (midicps note)
       detune (* 0.005 freq)
       src1 (sin-osc (- freq detune))
       src2 (sin-osc (+ freq detune))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* (+ src1 src2) env (* 0.5 vol))))

(beep 65)

(defn my-play [time tones sep]
  (let [tone (first tones)]
    (at time
      (beep (:note tone) (:vol tone)))
    (let [next-time (+ time sep)]
      (apply-at next-time my-play next-time (rest tones) sep []))))

(def rhythm  (flatten (repeat 4 '(1 1 1 1 1 1 1 1))))

(def tones (flatten (map #(map (fn [p] (+ p (* 12 %)))
                               (degrees->pitches '(:i :ii :iii :iv :v :vii) :aeolian :D3)) '(0 1 2 3))))

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
                     :vol 0}
                    {:note (first tones)
                     :vol (* vol (accfn n))})
             tones-tail (if (> (first rhythm) 0)
                          (rest tones)
                          tones)]
         (recur (conj result tone) rhythm-tail tones-tail (inc n) accfn)))))

;;(my-play (now) (melody rhythm tones (fn [n] (if (zero? (mod n 4)) 1 0.5)))  100)

(defn accfn [n]
  (if (zero? (mod n 3)) 1.0 0.1))

;;(my-play (now) (flatten (repeatedly (fn [] (melody rhythm tones accfn)))) 100)

(stop)
