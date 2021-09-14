(ns zen.ops.resource)

(declare do-expand)

(defmulti expand (fn [ztx resource] (:zo/type resource)))

(defn do-expand [ztx resource]
  (->> (expand ztx resource)
       (reduce (fn [acc res]
                 (if (:zo/type res)
                   (into acc (do-expand ztx res))
                   (conj acc res)))
               [])))

