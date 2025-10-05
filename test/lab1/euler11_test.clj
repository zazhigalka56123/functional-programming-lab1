(ns lab1.euler11-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab1.euler11 :as euler11]
            [lab1.euler11-util :as util])
  (:import [java.util ArrayList]))

(def test-grid (util/generate-random-grid))

(defn- clojure-grid-to-java
  [grid]
  (let [java-grid (ArrayList.)]
    (doseq [row grid]
      (let [java-row (ArrayList.)]
        (doseq [num row]
          (.add java-row (int num)))
        (.add java-grid java-row)))
    java-grid))

(defn- run-euler11-java
  [grid]
  (lab1.Euler11/solve (clojure-grid-to-java grid) 4))

(def java-result (run-euler11-java test-grid))

(deftest consistency-test
  (testing "Проверка, что все реализации Clojure соответствуют результату Java."

    (testing "1. Хвостовая рекурсия"
      (is (= java-result (euler11/solve-tail-rec test-grid))))

    (testing "2. Обычная рекурсия"
      (is (= java-result (euler11/solve-rec test-grid))))

    (testing "3. Модульная"
      (is (= java-result (euler11/solve-modular test-grid))))

    (testing "4. Цикл"
      (is (= java-result (euler11/solve-loop test-grid))))

    (testing "5. Ленивые последовательности"
      (is (= java-result (euler11/solve-lazy test-grid))))))