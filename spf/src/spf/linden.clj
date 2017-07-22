(ns spf.linden
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(def initial '[A - - - - - -  B])

(def rules '{A [+ B - - A +]
             B [+ C - - A +]
             C [- C + + D -]
             D [- C + + A -]})


(defn cs [x]
  (case x
    A (q/color 22 32 143)
    B (q/color 42 132 43)
    C (q/color 162 32 43)
    D (q/color 182 32 143)))

;cs

(defn step! [path] (mapcat (fn [x] (get rules x [x])) path))

(defn setup []
  (q/frame-rate 4)
  {:path initial, :cnt 1})

(defn update-state [state] (update state :cnt inc))

(defn draw-path [path]
  (q/push-matrix)
  (doseq [x path
          :let [r 600]]
    (case x
      +   (q/rotate (q/radians -30.0))
      -   (q/rotate (q/radians +30.0))
      (do (q/stroke (cs x)) (q/line 0 0 0 r) (q/translate 0 r))))
  (q/pop-matrix))

(defn taker [counter x]
  (let [|x| (count x)
        idx (rem counter (* 2 |x|))
        i (if (>= idx |x|) (- (* 2 |x|) idx) idx)]
    (take (inc i) x)))

(defn draw-state [state]
  (q/background 255)
  (q/translate 480 580)
  (q/rotate (q/radians 180))

  (let [s (/ (q/sqrt 3))
        path (:path state)]
    (doseq [p (taker (:cnt state) (take 12 (iterate step! path)))]
      (q/scale s s)
      (draw-path p))))

(defstate sketch
  :start (q/sketch :size [960 960]
                   :setup #'setup
                   :update #'update-state
                   :draw #'draw-state
                   :features [:keep-on-top]
                   :middleware [m/fun-mode])
  :stop (.exit sketch))

;; (mount.core/stop)
;; (mount.core/start #'spf.linden/sketch)
