(ns lab1.euler19
  (:gen-class)
  (:require [lab1.euler19-util :as euler19-util]))

(def months (range 1 13))

; ============== 1. Монолитная реализация с хвостовой рекурсией ==============
(defn solve-tail-rec [start-year end-year]
  (letfn [(iter [year month day-of-week-idx sundays-count]
            (if (> year end-year)
              sundays-count
              (let [new-sundays (if (= day-of-week-idx 6) (inc sundays-count) sundays-count)
                    days-in-month (euler19-util/days-in-month year month)
                    new-day-idx (mod (+ day-of-week-idx days-in-month) 7)
                    next-month (if (= month 12) 1 (inc month))
                    next-year (if (= month 12) (inc year) year)]
                (recur next-year next-month new-day-idx new-sundays))))]
    (iter start-year 1 1 0)))

; ============== 2. Монолитная реализация с обычной рекурсией ==============
(defn solve-rec [start-year end-year]
  (letfn [(iter [year month day-of-week-idx]
            (if (<= year end-year)
              (let [is-sunday (if (= day-of-week-idx 6) 1 0)
                    days-in-month (euler19-util/days-in-month year month)
                    new-day-idx (mod (+ day-of-week-idx days-in-month) 7)
                    next-month (if (= month 12) 1 (inc month))
                    next-year (if (= month 12) (inc year) year)]
                (+ is-sunday (iter next-year next-month new-day-idx)))
              0))]
    (iter start-year 1 1)))

; ============== 3. Модульный подход ==============
(defn solve-modular [start-year end-year]
  (let [all-months (for [year (range start-year (inc end-year)) month months] [year month])
        days-in-months (map #(apply euler19-util/days-in-month %) all-months)
        first-day-indices (reductions #(mod (+ %1 %2) 7) 1 days-in-months)]
    (count (filter #(= 6 %) first-day-indices))))

; ============== 4. Использование цикла ==============
(defn solve-loop [start-year end-year]
  (loop [year start-year, month 1, day-idx 1, sundays 0]
    (if (> year end-year)
      sundays
      (let [new-sundays (if (= day-idx 6) (inc sundays) sundays)
            days-in-month (euler19-util/days-in-month year month)
            new-day-idx (mod (+ day-idx days-in-month) 7)
            next-month (if (= month 12) 1 (inc month))
            next-year (if (= month 12) (inc year) year)]
        (recur next-year next-month new-day-idx new-sundays)))))

; ============== 5. Ленивые последовательности ==============
(defn solve-lazy [start-year end-year]
  (let [all-dates (iterate
                   (fn [[year month day-idx]]
                     (let [days (euler19-util/days-in-month year month)
                           next-month (if (= month 12) 1 (inc month))
                           next-year (if (= month 12) (inc year) year)]
                       [next-year next-month (mod (+ day-idx days) 7)]))
                   [start-year 1 1])
        year-range (set (range start-year (inc end-year)))]
    (->> all-dates
         (take-while (fn [[year _ _]] (year-range year)))
         (filter (fn [[_ _ day-idx]] (= day-idx 6)))
         (count))))