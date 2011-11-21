(ns visibility-polygon.core
  (:require [visibility-polygon.seg :as seg]
            [visibility-polygon.pt :as pt])
  (:use [clojure.algo.monads]))


(defmonad polygon-parser-m
  [m-result (fn [v]
              (fn [pt poly stack]
                [pt (vec poly) (vec stack)]))
   m-bind (fn [mv f]
            (fn [pt poly stack]
              (let [result (mv pt poly stack)]
                (when (not= nil result)
                  (let [[pt new-poly new-stack] result]
                    ((f nil) pt new-poly new-stack))))))
   m-zero (fn [pt poly stack]
            nil)
   m-plus (fn [& parsers]
            (fn [pt poly stack]
              (drop-while nil?
                          (map #(% pt poly stack) parsers))))])

(def visible? (complement pt/left-turn?))

(defn add-new-pt [pt poly stack]
  (when (visible? pt (first poly) (first stack))
    [pt (rest poly) (vec (cons (first poly) stack))]))

(defn pop-stack [pt poly stack]
  (let [the-seg (seg/new-seg pt (first poly))
        top-seg (seg/new-seg (first stack) (second stack))]
    (when (pt/left-turn? (second stack) (first stack) (first poly))
      (if (intersection-on-seg? the-seg top-seg)
        [pt poly (cons (seg/intersection the-seg top-seg) stack)]
        [pt poly (rest stack)]))))

(defn skip-pt [pt poly stack]
  (let [the-seg (seg/new-seg pt (first stack))
        poly-seg (seg/new-seg (first poly) (second poly))]
    (when (not (pt/left-turn? (second stack) (first stack) (first poly)))
      (if (intersection-on-seg? the-seg poly-seg)
        [pt (cons (seg/intersection the-seg poly-seg) (rest poly)) stack]
        [pt (rest poly) stack]))))

