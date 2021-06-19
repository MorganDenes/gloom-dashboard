(ns gloom-dashboard.util
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [clojure.pprint :as pprint]))

(defn h "Half the value" [v] (try (/ v 2) (catch Exception e (pprint/pprint e) (throw e))))
(defn nh "Negative half value" [v] (try (- (h v)) (catch Exception e (pprint/pprint e) (throw e))))

(defn -cy
  "Create a cylinder with a higher resolution"
  [r h]
  (->> (scad-clj.model/cylinder (* r 5) h)
       (scale [(/ 1 5) (/ 1 5) 1])))

(defn -cu
  [size location]
  (let [[x y z] size]
    (->> (cube x y z)
         (translate location))))

(defn -ttlcu
  [size location]
  (let [[x y z] size]
    (translate [(h x) (nh y) (nh z)] (-cu size location))))

(defn -ltrap
  [size location]
  (let [[x1 x2 y z] size
        [xx yy zz] location
        inter (cube x2 y z)]
    (translate [xx yy zz]
               (intersection
                inter
                (hull (-cu [x1 y z] [(nh (- x2 x1)) 0 0])
                      (-cu [x2 y z] [0 0 (- z)]))))))

(defn -trap
  [size location]
  (let [[x1 x2 y z] size
        [xx yy zz] location
        inter (cube x2 y z)]
    (translate [xx yy zz] (intersection
                           inter
                           (hull (cube x1 y z)
                                 (-cu [x2 y z] [0 0 (- z)]))))))










