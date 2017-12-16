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

(defn- spidron-arm-pts [x1 y1 x2 y2 left?]
  (let [sc    (* sqrt3-1 (Math/hypot (- x1 x2) (- y1 y2)))
        angle (+ (Math/atan2 (- y1 y2) (- x2 x1)) (/ Math/PI 3) (if left? deg60 0))]
    (for [i (range 8)
          :let [rotation (+ angle (* i deg30))
                scaling  (* sc (Math/pow sqrt3-1 i))]]
      [(+ x1 (* scaling (Math/sin rotation)))
       (+ y1 (* scaling (Math/cos rotation)))])))

(defn spidron-arm-side [x1 y1 x2 y2 left? n]
  (if-not (pos? n)
    [[x1 y1] [x2 y2]]
    (let [pts  (spidron-arm-pts x1 y1 x2 y2 left?)
          ys   (mapcat (fn [[a b] [c d]] (spidron-arm-side a b c d false (dec n)))
                       pts (next pts))

          pts2 (spidron-arm-pts x2 y2 x1 y1 (not left?))
          zs   (mapcat (fn [[a b] [c d]] (spidron-arm-side a b c d false (dec n)))
                       pts2 (next pts2))]
      (concat (reverse ys) (next zs)))))


(defn draw-lines [pts]
  (doseq [[[x1 y1] [x2 y2]] (map vector pts (next pts))] (q/line x1 y1 x2 y2)))

(def STEPS 3)

(defn spidron-arm [x1 y1 x2 y2]
  (q/stroke 255 0 0)
  (draw-lines (spidron-arm-side x1 y1 x2 y2 false STEPS))
  (q/stroke 0 0 244)
  (draw-lines (spidron-arm-side x1 y1 x2 y2 true STEPS)))

(defn update-state [s] s)

(defn draw-state [state]
  (q/background 233 243 55)
  (try (spidron-arm 200 200 544 555) (catch Exception e nil)))

(defstate sketch
  :start (future (q/sketch :size [960 960]
                    :setup #'setup
                    :update #'update-state
                    :draw #'draw-state
                    :features [:keep-on-top]
                    :middleware [m/fun-mode]))
  :stop (future (.exit @sketch)))

; (mount.core/start spf.constructive/sketch)
