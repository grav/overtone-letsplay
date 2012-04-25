(ns betafunk.letsplay
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

;; (def tones [65 66 67])
;; (loop-play (now) tones 200)0
;; redefining tones will work, ie
;; (def tones [67 66 65]

(defn updown [tones]
  (concat tones  (reverse (drop 1 (take (dec (count tones)) tones)))))

(def tones
  (degrees->pitches [:i :iii :v :vii] :ionian :F3))


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

;;(ctl myplay :sep 200)
