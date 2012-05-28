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

;; TODO - this should be more dynamic
(definst noise1 [vol 1] (* vol (quickfree "ding" 3)))

(definst noise2 [vol 1]  (* vol (quickfree "snap" 7 3)))

(definst noise3 [vol 1] (* vol (quickfree "cluck" 1)))

(definst noise4 [vol 1] (* vol (quickfree "pling" 6 3)))

(def noises (list noise1 noise2 noise3 noise4))

;; initial beep
(definst beep [note 60]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))


(def tones)

(def metro (metronome 160))


;; (drumbeat (metro) 0)
;; (my-play (metro) (flatten (repeatedly (fn [] tones))))
