(ns spf.ablak)


(def ablak
  (doto (new javax.swing.JFrame)
    (.setSize 400 300)
    (.show)
    ))

(def gomb (new javax.swing.JButton "gomb"))

(.add ablak gomb)

(.setBackground gomb java.awt.Color/RED)

[1 2 3 4]

{1 "elso elem",
 2 "masodik elem"}

#{1 2 3 4 5}

:adsadasd

(def a [1 2 3 4])

(defn szamolj [n]
  (let [ertekek (range n)]
    (reduce + ertekek)
    ))

(szamolj 100)

(dotimes [i 100]
  (println i))
