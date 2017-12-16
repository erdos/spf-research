(ns spf.constructive
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(defn setup [] (q/frame-rate 4) {})

(def deg30 (/ Math/PI 6))
(def deg60 (/ Math/PI 3))
(def deg120 (* deg60 2))
(def sqrt3-1 (/ (Math/sqrt 3)))

(defn spidron-arm-half [x y angle sc]
  (let [angle (+ angle (/ Math/PI 3))]
    (q/begin-shape :triangle-strip)
    (dotimes [i 10]
      (let [rotation (+ angle (* i deg30))
            scaling  (* sc (Math/pow sqrt3-1 i))]
        (q/vertex (+ x (* scaling (Math/sin rotation)))
                  (+ y (* scaling (Math/cos rotation))))
        (q/vertex (+ x (* scaling (Math/sin (+ rotation deg60))))
                  (+ y (* scaling (Math/cos (+ rotation deg60)))))))
    (q/end-shape)))

(defn spidron-arm [x1 y1 x2 y2]
  (let [dist (Math/hypot (- x1 x2) (- y1 y2))
        r    (/ dist 2 (Math/cos (/ Math/PI 6)))
        d    (Math/atan2 (- y1 y2) (- x2 x1))]
    (q/fill 255 255 255)
    (spidron-arm-half x1 y1 d r)
    (spidron-arm-half x2 y2 (- d Math/PI) r)))

(defn update-state [s] s)

(defn draw-state [state]
  (q/background 233 243 155)
  (spidron-arm 200 200 544 555))

(defstate sketch
  :start (q/sketch :size [960 960]
                   :setup #'setup
                   :update #'update-state
                   :draw #'draw-state
                   :features [:keep-on-top]
                   :middleware [m/fun-mode])
  :stop (.exit sketch))

; (mount.core/start spf.constructive/sketch)
