(ns json-patch.diff
  (:require [clojure.string :as str]
            [json-patch.path :refer [path-str butlastv]]))

(defn primitive? [x]
  (and (not (map? x)) (not (sequential? x))))

(defn push-path [ctx k]
  (update ctx :path conj k))

(defn pop-path [ctx]
  (update ctx :path butlastv))

(defn add-op [ctx op]
  (update ctx :tr conj (assoc op :path (path-str (:path ctx)))))

(declare *diff)

(defn mk-reduce [a b]
  (fn [ctx k]
    (-> ctx
        (push-path k)
        (*diff (get a k) (get b k))
        (pop-path))))

(defn all-keys [a b]
  (into #{} (concat (keys a) (keys b))))

(defn all-idx [a b]
  (range 0 (max (count a) (count b))))

(defn *diff [ctx a b]
  (if (= a b)
    ctx
    (let [a-nil? (nil? a)
          b-nil? (nil? b)
          a-prim? (primitive? a)
          b-prim? (primitive? b)]

      (cond

        (and (not a-nil?) b-nil?)
        (add-op ctx {:op "remove"})

        (and a-nil? (not b-nil?))
        (add-op ctx {:op "add" :value b})

        (and (map? a) (map? b))
        (->> (all-keys a b)
             (reduce (mk-reduce a b) ctx))

        (and (sequential? a) (sequential? b))
        (->> (all-idx a b)
             (reduce (mk-reduce a b) ctx ))

        (and (not a-nil?) (not b-nil?))
        (add-op ctx {:op "replace" :value b})

        :else (throw (Exception. (str "Unexpected " a " " b)))))))

(defn diff [a b]
  (:tr (*diff {:tr [] :path []} a b)))
