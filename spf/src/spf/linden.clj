(ns spf.linden
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(def axiom '[A - - - - - -  B])

(def rules '{A [+ B - - A +]
             B [+ C - - A +]
             C [- C + + D -]
             D [- C + + A -]})

(def ROTATION (q/radians -30))
(def SCALING (/ (q/sqrt 3)))

(defn cs [x]
  (case x
    A (q/color 22 32 143)
    B (q/color 42 132 43)
    C (q/color 162 32 43)
    D (q/color 182 32 143)))

(def step! (partial mapcat #(get rules % [%])))

; (mapcat #(list "!" % % ">") [1 2 3])

(defn setup []
  (q/frame-rate 4)
  {:path axiom, :cnt 1})

(defn update-state [state] (update state :cnt inc))

(defn draw-path [path]
  (q/push-matrix)
  (doseq [x path :let [r 600]]
    (case x
      +   (q/rotate ROTATION)
      -   (q/rotate (- ROTATION))
      (do (q/stroke (cs x))
          (q/line 0 0 0 r)
          (q/translate 0 r))))
  (q/pop-matrix))

(defn taker [counter x]
  (let [|x| (count x)
        idx (rem counter (* 2 |x|))
        i (if (>= idx |x|) (- (* 2 |x|) idx) idx)]
    (take (inc i) x)))

(defn draw-state [state]
  (q/background 255)
  (q/translate 480 780)
  (q/rotate (q/radians 180))
  (q/stroke-weight 4)
  (q/scale 2 2)

  (let [path (:path state)]
    (doseq [p (taker (:cnt state) (take 12 (iterate step! path)))]
      (q/scale SCALING SCALING)
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
