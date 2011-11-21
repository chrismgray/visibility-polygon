(ns visibility-polygon.test.core
  (:use [visibility-polygon.core])
  (:require [visibility-polygon.pt :as pt])
  (:use [clojure.test]))

(deftest test-fix-poly
  (let [poly (map #(pt/new-pt (first %) (second %))
                  [[-3 2]
                   [0 -1]
                   [2 1]
                   [4 -1]
                   [7 2]])
        good-poly (map #(pt/new-pt (first %) (second %))
                       [[1 0]
                        [2 1]
                        [4 -1]
                        [7 2]
                        [-3 2]
                        [0 -1]])
        pt (pt/new-pt 0 0)]
    (is (= (vec good-poly)
           (vec (fix-poly pt poly))))))


(deftest test-visibility-polygon
  (let [poly (map #(pt/new-pt (first %) (second %))
                  [[-3 2]
                   [0 -1]
                   [2 1]
                   [4 -1]
                   [7 2]])
        vis-poly (map #(pt/new-pt (first %) (second %))
                      [[1 0]
                       [2 1]
                       [4 2]
                       [-3 2]
                       [0 -1]])
        pt (pt/new-pt 0 0)]
    (is (= (vec vis-poly)
           (visibility-polygon pt poly)))))
