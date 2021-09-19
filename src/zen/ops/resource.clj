(ns zen.ops.resource
  (:require
   [zen.core :as zen]
   [clojure.walk :as walk])
  (:refer-clojure :exclude [format]))

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
                 (let [fres (do-format ztx res)]
                   (when-let [tp (or (:zo/type fres) (:k8s/type fres))]
                     (->>
                      (zen/validate ztx #{tp} (dissoc fres :zo/type :k8s/type))
                      :errors
                      (mapv (fn [err] (println :expansion-error tp err)))))
                   (if (:zo/type fres)
                     (into acc (do-expand ztx fres))
                     (conj acc fres))))
               [])))



