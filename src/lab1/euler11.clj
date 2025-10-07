(ns lab1.euler11
  (:gen-class)
  (:require [lab1.euler11-util :as util]))

; ============== 1. Монолитная реализация с хвостовой рекурсией ==============
(defn solve-tail-rec
  [grid]
  (let [height (count grid)
        width (count (first grid))
        len 4]
    (letfn [(iter [i j max-p]
              (if (>= i height)
                max-p
                (let [new-j (if (= j (dec width)) 0 (inc j))
                      new-i (if (= j (dec width)) (inc i) i)
                      h-prod (when (<= (+ j len) width) (util/product (for [k (range len)] (get-in grid [i (+ j k)]))))
                      v-prod (when (<= (+ i len) height) (util/product (for [k (range len)] (get-in grid [(+ i k) j]))))
                      d1-prod (when (and (<= (+ i len) height) (<= (+ j len) width))
                                (util/product (for [k (range len)] (get-in grid [(+ i k) (+ j k)]))))
                      d2-prod (when (and (<= (+ i len) height) (>= (- j (dec len)) 0))
                                (util/product (for [k (range len)] (get-in grid [(+ i k) (- j k)]))))
                      current-max (max (or h-prod 0) (or v-prod 0) (or d1-prod 0) (or d2-prod 0))]
                  (recur new-i new-j (max max-p current-max)))))]
      (iter 0 0 0))))

; ============== 2. Монолитная реализация с обычной рекурсией ==============
(defn solve-rec
  [grid]
  (let [height (count grid), width (count (first grid)), len 4]
    (letfn [(iter [i j]
              (if (>= i height)
                0
                (let [new-j (if (= j (dec width)) 0 (inc j))
                      new-i (if (= j (dec width)) (inc i) i)
                      h-prod (when (<= (+ j len) width) (util/product (for [k (range len)] (get-in grid [i (+ j k)]))))
                      v-prod (when (<= (+ i len) height) (util/product (for [k (range len)] (get-in grid [(+ i k) j]))))
                      d1-prod (when (and (<= (+ i len) height) (<= (+ j len) width))
                                (util/product (for [k (range len)] (get-in grid [(+ i k) (+ j k)]))))
                      d2-prod (when (and (<= (+ i len) height) (>= (- j (dec len)) 0))
                                (util/product (for [k (range len)] (get-in grid [(+ i k) (- j k)]))))
                      current-max (max (or h-prod 0) (or v-prod 0) (or d1-prod 0) (or d2-prod 0))]
                  (max current-max (iter new-i new-j)))))]
      (iter 0 0))))

; ============== 3. Модульный подход с разделением по направлениям ==============
(defn- get-sequences-from-vector [v len]
  (when (>= (count v) len) (partition len 1 v)))

(defn- get-horizontal-sequences [grid len]
  (mapcat #(get-sequences-from-vector % len) grid))

(defn- get-vertical-sequences [grid len]
  (get-horizontal-sequences (apply mapv vector grid) len))

(defn- get-diagonal-sequences [grid len]
  (let [h (count grid), w (count (first grid))]
    (concat
     (for [i (range (- h (dec len))) j (range (- w (dec len)))]
       (vec (for [k (range len)] (get-in grid [(+ i k) (+ j k)]))))
     (for [i (range (- h (dec len))) j (range (dec len) w)]
       (vec (for [k (range len)] (get-in grid [(+ i k) (- j k)])))))))

(defn solve-modular
  [grid]
  (let [len 4]
    (->> (lazy-cat
          (get-horizontal-sequences grid len)
          (get-vertical-sequences grid len)
          (get-diagonal-sequences grid len))
         (map util/product)
         (apply max))))

; ============== 4. Использование цикла ==============
(defn solve-loop
  [grid]
  (let [height (count grid), width (count (first grid)), len 4]
    (loop [i 0, j 0, max-p 0]
      (if (>= i height)
        max-p
        (let [new-j (if (= j (dec width)) 0 (inc j))
              new-i (if (= j (dec width)) (inc i) i)
              h-prod (when (<= (+ j len) width) (util/product (for [k (range len)] (get-in grid [i (+ j k)]))))
              v-prod (when (<= (+ i len) height) (util/product (for [k (range len)] (get-in grid [(+ i k) j]))))
              d1-prod (when (and (<= (+ i len) height) (<= (+ j len) width))
                        (util/product (for [k (range len)] (get-in grid [(+ i k) (+ j k)]))))
              d2-prod (when (and (<= (+ i len) height) (>= (- j (dec len)) 0))
                        (util/product (for [k (range len)] (get-in grid [(+ i k) (- j k)]))))
              current-max (max (or h-prod 0) (or v-prod 0) (or d1-prod 0) (or d2-prod 0))]
          (recur new-i new-j (max max-p current-max)))))))

; ============== 5. Ленивый подход с одним генератором ==============
(defn solve-lazy
  [grid]
  (let [height (count grid)
        width (count (first grid))
        len 4
        all-sequences
        (lazy-cat
         (for [r (range height), c (range (- width (dec len)))]
           (vec (for [k (range len)] (get-in grid [r (+ c k)]))))
         (for [r (range (- height (dec len))), c (range width)]
           (vec (for [k (range len)] (get-in grid [(+ r k) c]))))
         (for [r (range (- height (dec len))), c (range (- width (dec len)))]
           (vec (for [k (range len)] (get-in grid [(+ r k) (+ c k)]))))
         (for [r (range (- height (dec len))), c (range (dec len) width)]
           (vec (for [k (range len)] (get-in grid [(+ r k) (- c k)])))))]
    (->> all-sequences
         (map util/product)
         (apply max))))

; ============== 6. Подход на основе трансформации данных (матриц) ==============
(defn solve-transformed
  [grid]
  (let [len 4
        grid-v (apply mapv vector grid)
        h (count grid)
        w (count (first grid))
        diagonals (concat
                   (for [i (range (- h (dec len))) j (range (- w (dec len)))]
                     (vec (for [k (range len)] (get-in grid [(+ i k) (+ j k)]))))
                   (for [i (range (- h (dec len))) j (range (dec len) w)]
                     (vec (for [k (range len)] (get-in grid [(+ i k) (- j k)])))))]
    (apply max
           (map util/product
                (lazy-cat (get-horizontal-sequences grid len)
                          (get-horizontal-sequences grid-v len)
                          diagonals)))))
