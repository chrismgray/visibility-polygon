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

(defn- add-new-pt [poly]
  (fn [pt stack]
    (when (or (empty? stack) ; First two points are guaranteed to be visible
              (empty? (rest stack))
              (visible? pt (first poly) (first stack)))
      [pt (rest poly) (cons (first poly) stack)])))

(defn- pop-stack [poly]
  (fn [pt stack]
    (let [the-seg (seg/new-seg pt (first poly))
          top-seg (seg/new-seg (first stack) (second stack))]
      (when (pt/left-turn? (second stack) (first stack) (first poly))
        (if (and (seg/intersection-on-seg? the-seg top-seg)
                 (seg/pt-on-seg? (seg/intersection the-seg top-seg) top-seg))
          [pt poly (cons (seg/intersection the-seg top-seg) stack)]
          [pt poly (rest stack)])))))

(defn- skip-pt [poly]
  (fn [pt stack]
   (let [the-seg (seg/new-seg pt (first stack))
         poly-seg (seg/new-seg (first poly) (second poly))]
     (when (not (pt/left-turn? (second stack) (first stack) (first poly)))
       (if (and (seg/intersection-on-seg? the-seg poly-seg)
                (seg/pt-on-seg? (seg/intersection the-seg poly-seg) poly-seg))
         [pt (cons (seg/intersection the-seg poly-seg) (rest poly)) stack]
         [pt (rest poly) stack])))))

(defn- all-conditions [poly]
  (with-monad polygon-parser-m
    (m-plus (add-new-pt poly) (pop-stack poly) (skip-pt poly))))

(defn- visibility-polygon-helper [pt poly]
  ((with-monad polygon-parser-m
     (m-until empty? all-conditions poly)) pt []))

(defn fix-poly
  "Takes a point and a poly and returns a new poly with one point added.
   That point that is added is the closest point to the input point that
   is on the boundary of the poly and is directly to the right of the input
   point."
  [pt poly]
  (let [segs (map seg/new-seg (cons (last poly) poly) poly)
        horizontal-seg (seg/new-seg pt (pt/new-pt (+ 1 (pt :x)) (pt :y)))
        best-seg (->> segs
                      (filter #(seg/intersection-on-seg? horizontal-seg %))
                      (filter #(seg/pt-on-seg? (seg/intersection % horizontal-seg) %))
                      (filter #(> (:x (seg/intersection horizontal-seg %)) (:x pt)))
                      (apply min-key #(:x (seg/intersection horizontal-seg %))))]
    (assert (= (:y pt) (:y (seg/intersection horizontal-seg best-seg))))
    (cons (seg/intersection horizontal-seg best-seg)
          (cons (:e2 best-seg)
                (->> (concat poly poly)
                     (drop-while #(not= % (:e2 best-seg)))
                     next
                     (take-while #(not= % (:e2 best-seg))))))))

(defn visibility-polygon
  "Finds the visibility polygon of pt within poly.  This is the subset
   of poly that pt can 'see', assuming that the edges of poly are opaque.
   poly must be a simple polygon (that is, no self-intersections) and
   it must be in counterclockwise order.  It is represented as a sequence
   of points (defined as in pt/new-pt)."
  [pt poly]
  (let [poly (fix-poly pt poly)
        [_ _ stack] (visibility-polygon-helper pt poly)]
    (vec (reverse stack))))
