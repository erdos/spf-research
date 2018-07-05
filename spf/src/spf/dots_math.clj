(ns spf.dots-math
  "Iterated Function System description"
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mount.core :refer [defstate]]))

(def STEPS 9)

(defn setup []
  (q/frame-rate 2)
  {:steps STEPS})

(defmacro with-matrix [& bodies]
  `(try (q/push-matrix) ~@bodies (finally (q/pop-matrix))))

(defn c [i] (* 30 i) #_(* 255 (Math/pow 2 (- i))))

(def ratio (/ (q/sqrt 3)))
(def sqrt3 (q/sqrt 3))

(defn rot [f [x y]]
  (let [s (q/sin f), c (q/cos f)]
    [(- (* c x) (* s y)) (+ (* c y) (* s x))]))

(defn sca [r [x y]] [(* r x) (* r y)])

(def supercat'
  (juxt (comp (partial rot (q/radians 30)) (partial sca ratio))
        (comp #(update % 1 inc) (partial rot (q/radians 150)) (partial sca ratio))
        (comp #(update % 1 inc) (partial rot (q/radians -150)) (partial sca ratio))))

(def supercat
  (juxt (fn f1 [[x y]] [(- (/ x 2) (/ y 2 sqrt3)) (+ (/ y 2) (/ x 2 sqrt3))])
        (fn f2 [[x y]] [(- 0 (/ x 2) (/ y 2 sqrt3)) (+ 1 (/ x 2 sqrt3) (/ y -2))])
        (fn f3 [[x y]] [(- (/ y 2 sqrt3) (/ x 2)) (- 1 (/ x 2 sqrt3) (/ y 2))])))

(defn calc-pts [i]
  (->> i (dec) (calc-pts) (mapcat supercat) (set) (if (<= i 0) [[0 0]])))

(defn dedupe [pts]
  pts
  #_(let [r (int (Math/round (* 4 (Math/sqrt (count pts)))))
        grp (group-by (fn [[x y]] [(int (* r x)) (int (* r y))]) pts)]
    (map first (vals grp))))

(defn update-state [state]
  (if (:points state) state
      (assoc state :points (dedupe (calc-pts (:steps state))))))

(defn make-point-groups [pts]
  (let [r 512
        idx (fn [[x y]] [(int (* r x)) (int (* r y))])
        gs (group-by idx pts)]
    (fn [pt]
      (count (get gs (idx pt))))))

(defn draw-state [state]
  (q/background 255)
  (with-matrix (q/translate (/ (q/width) 2) (/ (q/height) 8))
    (q/stroke-weight 0.8)
    (q/stroke 24 24 24)
    (q/no-fill)
    (q/fill 12 12 12 128)
    (let [n (:steps state)
          ;pt-group (make-point-groups (:points state))
          cr (* 1000 (Math/pow sqrt3 (- n)))]
      (doseq [[x y] (:points state)
              :let [r 720]]
        (q/ellipse  (* r x) (* r y) cr cr)))))

(defstate sketch
  :start (q/sketch :size [960 960]
                   :setup #'setup
                   :update #'update-state
                   :draw #'draw-state
                   :features [:keep-on-top]
                   :middleware [m/fun-mode])
  :stop (.exit sketch))

;; (mount.core/stop)
;; (mount.core/start #'spf.dots-math/sketch)
