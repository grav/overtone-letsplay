(ns letsplay
  (:use [overtone.live]))

(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env vol)))

(defn my-play [time tones sep]
  (let [tone (first tones)]
    (at time
      (beep (:note tone) (:vol tone)))
    (let [next-time (+ time sep)]
      (apply-at next-time my-play next-time (rest tones) sep []))))

(def rhythm (flatten (repeat '(0 1 1 0 1 0 1 0))))

(def tones (flatten (map #(map (fn [p] (+ p (* 12 %))) (degrees->pitches '(:i :iii :iv :v :vii) :aeolian :F2)) '(0 1 2 3))))


(defn melody [result rhythm tones]
  (if (nil? (first tones))
    result
    (let [rhythm-tail (rest rhythm)
          tone {:note (first tones) :vol (first rhythm)}
          tones-tail (if (> (first rhythm) 0)
                      (rest tones)
                      tones)]
      (recur (conj result tone) rhythm-tail tones-tail))))

;;(my-play (now) (melody [] rhythm tones) 100)

;;(my-play (now) (flatten (repeatedly (fn [] (melody [] rhythm tones)))) 100)

(stop)
