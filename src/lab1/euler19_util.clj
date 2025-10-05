(ns lab1.euler19-util)

(defn leap-year?
  [year]
  (or (and (zero? (mod year 4)) (not= 0 (mod year 100)))
      (zero? (mod year 400))))

(defn days-in-month
  [year month]
  (cond
    (contains? #{4 6 9 11} month) 30
    (= month 2) (if (leap-year? year) 29 28)
    :else 31))

(defn generate-random-year-range
  []
  (let [start-year (+ 1900 (rand-int 100))
        year-span (+ 10 (rand-int 91))
        end-year (+ start-year year-span)]
    [start-year end-year]))