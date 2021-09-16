(ns zen.ops.resource
  (:require [clojure.walk :as walk]))

(declare do-expand)

(defmulti format (fn [ztx fmt resource] fmt))
(defn do-format [ztx node]
  (->> node
       (walk/postwalk (fn [x]
                        (if-let [fmt (and (map? x) (:zo/fmt x))]
                          (format ztx fmt (dissoc x :zo/fmt :zo/sch))
                          x)))))


(defmulti expand (fn [ztx resource] (:zo/type resource)))

(defn do-expand [ztx resource]
  (->> (expand ztx resource)
       (reduce (fn [acc res]
                 (if (:zo/type res)
                   (into acc (do-expand ztx (do-format ztx res)))
                   (conj acc (do-format ztx res))))
               [])))



