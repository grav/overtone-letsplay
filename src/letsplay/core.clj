(ns letsplay
  (:use overtone.live))

(definst beep [note 60]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

;; one way of

(defn my-play [time tones sep]
  (let [tone (first tones)]
    (at time
        (beep (+ 0 tone)))
    (let [next-time (+ time sep)]
      (apply-at next-time my-play next-time (rest tones) sep []))))

(defmacro loop-play [time tones sep]
  (list 'my-play time (list 'flatten (list 'repeatedly (list 'fn [] tones))) sep))

;; (def tones [65 66 67]
;; (loop-play (now) tones 200)
;; redefining tones will work, ie
;; (def tones [67 66 65]

;; another way
(defn tonesfn []
  '(73 68 71))

(defn my-play2 [time tones sep])

;; (my-play2 (now) (flatten (repeatedly #'tonesfn)) 200)
;; redefining tonesfn will work, ie
;; (defn tonesfn [] '(71 68 72))



(for [n '(65 67 71)] (beep n))




(defn myplay [time notes sep]
  (let [note (first notes)]
    (when note
      (at time
          (beep note))
      (let [next-time (+ time sep)]
        (apply-at next-time myplay [next-time (rest notes) sep])))))

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
