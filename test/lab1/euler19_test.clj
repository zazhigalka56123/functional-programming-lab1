(ns lab1.euler19-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab1.euler19 :as euler19]
            [lab1.euler19-util :as euler19-util]))

(def test-years (euler19-util/generate-random-year-range))
(def start-year (first test-years))
(def end-year (second test-years))

(defn- run-euler19-java
  [start end]
  (lab1.Euler19/solve start end))

(def java-result (run-euler19-java start-year end-year))

(deftest consistency-test
  (testing (str "Проверка, что все реализации Clojure соответствуют результату Java."
                start-year " - " end-year ".")

    (testing "1. Хвостовая рекурсия"
      (is (= java-result (euler19/solve-tail-rec start-year end-year))))

    (testing "2. Обычная рекурсия"
      (is (= java-result (euler19/solve-rec start-year end-year))))

    (testing "3. Модульная"
      (is (= java-result (euler19/solve-modular start-year end-year))))

    (testing "4. Цикл"
      (is (= java-result (euler19/solve-loop start-year end-year))))

    (testing "5. Ленивые последовательности"
      (is (= java-result (euler19/solve-lazy start-year end-year))))))