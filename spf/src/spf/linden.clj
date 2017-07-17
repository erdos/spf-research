(ns spf.linden
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(def initial '[A - B - - C + D - E - F - G - H])

(def rules
  '{A [b G - H b]
    B [b A - B b]
    C [b - C + D - b]
    D [b - - E + H + + E - F - b]
    E [b G - H b]
    F [b A - B b]
    G [b - C + D - b]
    H [b F - G b]})

(defn step! [path] (mapcat (fn [x] (get rules x [x])) path))

(defn setup []
  (q/frame-rate 4)
  {:path initial, :cnt 1})

(defn update-state [state] (update state :cnt inc))

(defn draw-path [path]
  (q/push-matrix)
  (doseq [x path
          :let [r 320]]
    (case x
      j   (q/rotate (q/radians +30.0))
      b   (q/rotate (q/radians -30.0))

      +   (q/rotate (q/radians -60.0))
      -   (q/rotate (q/radians +60.0))
      (do (q/line 0 0 0 r) (q/translate 0 r))))
  (q/pop-matrix))

(defn taker [counter x]
  (let [|x| (count x)
        idx (rem counter (* 2 |x|))
        i (if (>= idx |x|) (- (* 2 |x|) idx) idx)]
    (take (inc i) x)))

(defn draw-state [state]
  (q/background 255)
  (q/translate 480 480)
  (q/rotate (q/radians 180))

  (let [s (/ (q/sqrt 3))
        path (:path state)]
    ;(draw-path (taker path))
    (doseq [p (taker (:cnt state) (take 10 (iterate step! path)))]
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
