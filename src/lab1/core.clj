(ns lab1.core
  (:gen-class)
  (:require [lab1.euler11 :as euler11]
            [lab1.euler11-util :as util]
            [lab1.euler19 :as euler19]
            [lab1.euler19-util :as euler19-util])
  (:import [java.util ArrayList]))

(def euler11-grid (util/generate-random-grid))
(def euler19-years (euler19-util/generate-random-year-range))

(defn- clojure-grid-to-java
  [grid]
  (let [java-grid (ArrayList.)]
    (doseq [row grid]
      (let [java-row (ArrayList.)]
        (doseq [num row]
          (.add java-row (int num)))
        (.add java-grid java-row)))
    java-grid))

(defn- check-result
  [expected actual]
  (if (= expected actual)
    (str actual " ✓")
    (str actual " ✗ (ожидалось " expected ")")))

(defn -main
  []
  (let [euler11-java-result (lab1.Euler11/solve (clojure-grid-to-java euler11-grid) 4)
        [start-year end-year] euler19-years
        euler19-java-result (lab1.Euler19/solve start-year end-year)]

    (println "========================================")
    (println "ЗАДАЧА 11 (Произведение в сетке)")
    (println "========================================")
    (println "Эталонный ответ (Java):" euler11-java-result)
    (println "\nРезультаты Clojure реализаций:")
    (println "1. Монолитная (хвост. рекурсия):"
             (check-result euler11-java-result (euler11/solve-tail-rec euler11-grid)))
    (println "2. Монолитная (обычн. рекурсия):" (check-result euler11-java-result (euler11/solve-rec euler11-grid)))
    (println "3. Модульная:" (check-result euler11-java-result (euler11/solve-modular euler11-grid)))
    (println "4. Цикл (loop/recur):" (check-result euler11-java-result (euler11/solve-loop euler11-grid)))
    (println "5. Ленивые последовательности:" (check-result euler11-java-result (euler11/solve-lazy euler11-grid)))

    (println "\n========================================")
    (println "ЗАДАЧА 19 (Подсчет воскресений)")
    (println "========================================")
    (println (str "Диапазон лет: " start-year " - " end-year))
    (println "Эталонный ответ (Java):" euler19-java-result)
    (println "\nРезультаты Clojure реализаций:")
    (println "1. Монолитная (хвост. рекурсия):"
             (check-result euler19-java-result (euler19/solve-tail-rec start-year end-year)))
    (println "2. Монолитная (обычн. рекурсия):"
             (check-result euler19-java-result (euler19/solve-rec start-year end-year)))
    (println "3. Модульная:" (check-result euler19-java-result (euler19/solve-modular start-year end-year)))
    (println "4. Цикл (loop/recur):" (check-result euler19-java-result (euler19/solve-loop start-year end-year)))
    (println "5. Ленивые последовательности:"
             (check-result euler19-java-result (euler19/solve-lazy start-year end-year))))

  (System/exit 0))