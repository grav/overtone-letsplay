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

;; some random noises from freesound
(definst noise1 [vol 1] (* vol (quickfree "ding" 3)))

(definst noise2 [vol 1]  (* vol (quickfree "snap" 7 3)))

(definst noise3 [vol 1] (* vol (quickfree "cluck" 1)))

(definst noise4 [vol 1] (* vol (quickfree "pling" 6 3)))

;; todo - more noises!

(def noises (list noise1 noise2 noise3 noise4))

;; initial beep
(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

;; todo - more interesting beep
;; ex: detune, cross-fade, several oscs, ...

(def metro (metronome 160))

(defn my-play
  ([tones] (let [next-beat (metro)]
             (my-play next-beat tones)))
  ([beat tones]
     (my-play beat tones 0))
  ([beat tones n]
     (let [tone (first tones)
           time (metro beat)
           divider 2]
       (at time
         (beep (:note tone) (:vol tone) (/ (mod n 7) 7)))
       (let [next-beat (+ (/ 1.0 divider) beat)]
         (apply-at (metro next-beat) my-play next-beat (rest tones) n [])))))

;; todo - rhythm, list of volumes

;; todo - tones, list of pitches

;; combine rhythm and tones
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
                    ;; subtract octave when 'ghost'
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

(defn drumbeat
  ([beat] (drumbeat beat 0))
  ([beat n]
     (let [time (metro beat)
           next-beat (+ 0.5 beat)]
       (if (zero? (mod n 8))
         (at time
           (kick)
           ))
       (if (weighted-coin 0.5)
         (at time
          ((rand-nth noises) (drum-acc n))))

       (apply-at (metro next-beat) #'drumbeat next-beat (+ 2 n) []))))

;; (my-play (flatten (repeatedly (fn [] (melody rhythm tones)))))

;; (drumbeat (metro))

;; (stop)
