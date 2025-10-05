(ns lab1.euler11-util)

(defn generate-random-grid
  []
  (vec (for [_ (range 20)]
         (vec (repeatedly 20 #(rand-int 100))))))

(defn product
  [coll]
  (reduce *' coll))