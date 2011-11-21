(ns visibility-polygon.core
  (:require [visibility-polygon.seg :as seg]
            [visibility-polygon.pt :as pt])
  (:use [clojure.algo.monads]))


(defmonad polygon-parser-m
  [m-result (fn [poly]
              (fn [pt stack]
                [pt (vec poly) (vec stack)]))
   m-bind (fn [mv f]
            (fn [pt stack]
              (let [result (mv pt stack)]
                (when (not= nil result)
                  (let [[pt new-poly new-stack] result]
                    ((f new-poly) pt new-stack))))))
   m-zero (fn [pt stack]
            nil)
   m-plus (fn [& parsers]
            (fn [pt stack]
              (some identity (map #(% pt stack) parsers))))])

(def visible? (complement pt/left-turn?))

(defn add-new-pt [poly]
  (fn [pt stack]
    (when (visible? pt (first poly) (first stack))
      [pt (rest poly) (vec (cons (first poly) stack))])))

(defn pop-stack [poly]
  (fn [pt stack]
    (let [the-seg (seg/new-seg pt (first poly))
          top-seg (seg/new-seg (first stack) (second stack))]
      (when (pt/left-turn? (second stack) (first stack) (first poly))
        (if (seg/intersection-on-seg? the-seg top-seg)
          [pt poly (cons (seg/intersection the-seg top-seg) stack)]
          [pt poly (rest stack)])))))

(defn skip-pt [poly]
  (fn [pt stack]
   (let [the-seg (seg/new-seg pt (first stack))
         poly-seg (seg/new-seg (first poly) (second poly))]
     (when (not (pt/left-turn? (second stack) (first stack) (first poly)))
       (if (seg/intersection-on-seg? the-seg poly-seg)
         [pt (cons (seg/intersection the-seg poly-seg) (rest poly)) stack]
         [pt (rest poly) stack])))))

(defn all-conditions [poly]
  (with-monad polygon-parser-m
    (m-plus (add-new-pt poly) (pop-stack poly) (skip-pt poly))))

(defn visibility-polygon-helper [pt poly]
  (with-monad polygon-parser-m
    (m-until #(seq %) all-conditions poly)) pt [])