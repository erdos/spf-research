(ns spf.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 2)
  ; Set color mode to HSB (HSV) instead of default RGB.
  ;; (q/color-mode :hsb)
  ; setup function returns initial state. It contains
                                        ; circle color and position.
  (q/no-smooth)
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defmacro with-matrix [& bodies]
  `(try (q/push-matrix) ~@bodies
        (finally (q/pop-matrix))))

(defn c [i]
  (* 30 i)
  #_(* 255 (Math/pow 2 (- i))))

(defn calc-pts
  ([i] (calc-pts i 0 0 0))
  ([i r g b]
   (if (<= i 0)
     (do (q/stroke (c r) (c g) (c b))
         (q/ellipse 0 0 1 1))
     (do
       (let [s (/ (q/sqrt 3))]
         (q/scale s s))
       (with-matrix
         (q/translate 0 2)
         (q/rotate (q/radians 150))
         (calc-pts (dec i) (inc r) g b))
       (with-matrix
         (q/translate 0 2)
         (q/rotate (q/radians -150))
         (calc-pts (dec i) r (inc g) b))
       (with-matrix
         (q/rotate (q/radians 30))
         (calc-pts (dec i) r g (inc b)))))))

(defn draw-state [state]
  (q/background 255)
  (q/fill (:color state) 55 55)
  (q/with-translation [(/ (q/width) 2)
                       (/ (q/height) 6)]
    (q/scale 300)
    (calc-pts 9)))

(defstate sketch
  :start
  (q/sketch
    :title "You spin my circle right round"
    :size [500 500]
    :setup #'setup
    :update #'update-state
    :draw #'draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode])
  :stop (.exit sketch))

;; (mount.core/stop)
;; (mount.core/start)
