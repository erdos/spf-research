(ns spf.linden
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(comment
  ;; original rules were:
  (def initial '[A + + B + C B + A + B + C + + B - - D])
  (def rules '{A [b +  H - -  I A b]
               B [j B + C j]
               C [j D + E j]
               D [j F + G j]
               E [b + H - - I A b]
               F [j B + C j]
               G [j D + E j]
               H [j F + G j]
               I [b D - G b]})
  )

(def initial '[D A + + B + C B + A + B + C + + B])

(def rules
  '{A [b + B - -  D A b]
    B [j B + C j]
    C [j B + A j]
    D [b B - C b]})

(defn cs [x]
  (case x
    A (q/color 22 32 43)
    B (q/color 42 32 43)
    C (q/color 62 32 43)
    D (q/color 82 32 43)
    E (q/color 102 32 43)
    F (q/color 122 32 43)
    G (q/color 142 32 43)
    H (q/color 162 32 43)
    I (q/color 182 32 43)))

;cs

(defn step! [path] (mapcat (fn [x] (get rules x [x])) path))

(defn setup []
  (q/frame-rate 4)
  {:path initial, :cnt 1})

(defn update-state [state] (update state :cnt inc))

(defn draw-path [path]
  (q/push-matrix)
  (doseq [x path
          :let [r 400]]
    (case x
      j   (q/rotate (q/radians +30.0))
      b   (q/rotate (q/radians -30.0))

      +   (q/rotate (q/radians -60.0))
      -   (q/rotate (q/radians +60.0))
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
