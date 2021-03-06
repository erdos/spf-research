(ns spf.constructive
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(defn setup [] (q/frame-rate 4) {})

(def deg30 (/ Math/PI 6))
(def deg60 (/ Math/PI 3))
(def deg120 (* deg60 2))
(def sqrt3-1 (/ (Math/sqrt 3)))

(defn- spidron-arm-pts [[x1 y1] [x2 y2] left?]
  (let [dist     (Math/hypot (- x1 x2) (- y1 y2))
        angle (+ (Math/atan2 (- y1 y2) (- x2 x1))
                 (if left? deg120 deg60))]
    (concat
     (for [i (range 8)
           :let [rotation (+ angle (* i deg30))
                 scaling  (* dist (Math/pow sqrt3-1 (inc i)))]
           :while (or (= 0 i) (> scaling 4))] ;; csak optimalizacio
       [(+ x1 (* scaling (Math/sin rotation)))
        (+ y1 (* scaling (Math/cos rotation)))])
     [[x1 y1]])))

(defn spidron-arm-side  [n [x1 y1 :as p1] [x2 y2 :as p2]]
  (if-not (pos? n)
    [[x1 y1] [x2 y2]]
    (let [pts  (spidron-arm-pts p1 p2 false)
          ys   (mapcat (partial spidron-arm-side (dec n)) pts (next pts))

          pts2 (spidron-arm-pts p2 p1 true)
          zs   (mapcat (partial spidron-arm-side (dec n)) pts2 (next pts2))]
      (concat (reverse ys) (next zs)))))

(defonce steps (atom 3))
                                        ;(swap! steps dec)
; (swap! steps inc)
(defn spidron-silhouette  [n [x1 y1 :as p1] [x2 y2 :as p2]]
  (concat (spidron-arm-side n [x1 y1] [x2 y2])
          (next (spidron-arm-side n [x2 y2] [x1 y1]))))

(defn draw-lines [pts]
  (doseq [[[x1 y1] [x2 y2]] (map vector pts (next pts))] (q/line x1 y1 x2 y2)))

(defn spidron-arm [x1 y1 x2 y2]
  (q/stroke 0)
  (draw-lines (spidron-silhouette 1 [x1 y1] [x2 y2]))

  ;(q/stroke 64)
  ;(draw-lines (spidron-silhouette 2 [x1 y1] [x2 y2]))

 ; (q/stroke 128)
 ; (draw-lines (spidron-silhouette 3 [x1 y1] [x2 y2]))

  (q/stroke 192 12 32)
  (draw-lines (spidron-silhouette 4 [x1 y1] [x2 y2]))

 ; (q/stroke 220)
 ; (draw-lines (spidron-silhouette 5 [x1 y1] [x2 y2]))

;  (q/stroke 240)
;  (draw-lines (spidron-silhouette 6 [x1 y1] [x2 y2]))

  )

(defn update-state [s] s)

(defn draw-state [state]
  (q/background 233 243 55)


  (let [inside #(try (spidron-arm 360 100 360 860)
                     (catch Exception e nil))]
    (inside)
    #_(q/do-record (q/create-graphics 960 960 :pdf "/home/erdos/aaa.pdf")
                 (inside)
                )))

(defstate sketch
  :start (future (q/sketch :size [960 960]
                    :setup #'setup
                    :update #'update-state
                    :draw #'draw-state
                    :features [:keep-on-top]
                    :middleware [m/fun-mode]))
  :stop (future (.exit @sketch)))

; (mount.core/start spf.constructive/sketch)
