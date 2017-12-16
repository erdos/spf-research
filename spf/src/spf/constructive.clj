(ns spf.constructive
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(defn setup [] (q/frame-rate 4) {})

(def deg30 (/ Math/PI 6))
(def deg60 (/ Math/PI 3))
(def deg120 (* deg60 2))
(def deg180 Math/PI)
(def sqrt3-1 (/ (Math/sqrt 3)))

(defn spidron-arm-half-pts [x y angle sc]
  (let [angle (+ angle (/ Math/PI 3))]
    (for [i (range 10)]
      (let [rotation (+ angle (* i deg30))
            scaling  (* sc (Math/pow sqrt3-1 i))]
        [(+ x (* scaling (Math/sin rotation)))
         (+ y (* scaling (Math/cos rotation)))]))))

(defn spidron-arm-pts [x1 y1 x2 y2]
  (let [dist (Math/hypot (- x1 x2) (- y1 y2))
        r    (/ dist 2 (Math/cos (/ Math/PI 6)))
        d    (Math/atan2 (- y1 y2) (- x2 x1))
        pts (spidron-arm-half-pts x1 y1 d r)
        pts2 (spidron-arm-half-pts x2 y2 (- d deg120) r)]
    (concat (reverse pts) (next pts2))))

(defn spidron-arm-pts* [n x1 y1 x2 y2]
  (if (pos? n)
    (let [pts (spidron-arm-pts* (dec n) x1 y1 x2 y2)]
      (for [[[x1 y1] [x2 y2]] (map vector pts (next pts))
            pt (spidron-arm-pts x1 y1 x2 y2)]
        pt))
    [[x1 y1] [x2 y2]]))

                                        ; (spidron-arm-pts* 1 0 0 100 100)

(def STEPS 1)

(defn spidron-arm-side [x1 y1 x2 y2]
  (let [pts (spidron-arm-pts* STEPS x1 y1 x2 y2)]
    (println :> pts)
    (doseq [[[x1 y1] [x2 y2]] (map vector pts (next pts))]
      (q/line x1 y1 x2 y2))))

(defn spidron-arm [x1 y1 x2 y2]
  (spidron-arm-side x1 y1 x2 y2)
  (spidron-arm-side x2 y2 x1 y1))

(defn update-state [s] s)

(defn draw-state [state]
  (q/background 233 243 55)
  ;(println :>>!!)
  (try (spidron-arm 200 200 544 555) (catch Exception e (println e))))

(defstate sketch
  :start (q/sketch :size [960 960]
                   :setup #'setup
                   :update #'update-state
                   :draw #'draw-state
                   :features [:keep-on-top]
                   :middleware [m/fun-mode])
  :stop (.exit sketch))

; (mount.core/start spf.constructive/sketch)
