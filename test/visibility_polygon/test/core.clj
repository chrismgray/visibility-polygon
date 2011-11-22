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

(deftest new-bad-poly
  (let [poly [{:x 162, :y 193} {:x 222, :y 231} {:x 203, :y 308} {:x 122, :y 315} {:x 157, :y 289} {:x 108, :y 258} {:x 163, :y 246} {:x 120, :y 208} {:x 155, :y 216}]
        pt {:x 140, :y 265}]
    (is (not (nil? (visibility-polygon pt poly))))))

(deftest bad-poly-2
  (let [poly [{:x 299, :y 175} {:x 322, :y 230} {:x 411, :y 228} {:x 479, :y 179} {:x 537, :y 254} {:x 525, :y 302} {:x 418, :y 339} {:x 273, :y 328} {:x 276, :y 256} {:x 322, :y 303}]
        pt {:x 292, :y 292}]
    (is (not (nil? (visibility-polygon pt poly))))))