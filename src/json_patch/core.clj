(ns json-patch.core
  (:require [clojure.string :as str]
            [cheshire.core]
            [json-patch.path :refer [parse-path path-str path-errors]]))

(defn to-json [x]
  (cheshire.core/generate-string x))

(defn *contains? [m k]
  (when (or (map? m) (sequential? m))
    (contains? m k)))

(defn *get [m k]
  (cond (map? m) (get m k)
        (and (sequential? m) (integer? k)) (nth m k nil)))

(defn *get-in [m ks]
  (reduce *get m ks))

(defn *assoc [m k v]
  (if (or (map? m) (sequential? m))
    (assoc m k v)
    (throw (Exception. (str "Could not *assoc into " (type m) " " (pr-str m))))))

(defn *assoc-in
  [m [k & ks] v]
  (if ks
    (*assoc m k (*assoc-in (*get m k) ks v))
    (*assoc m k v)))


(defn add-error [ctx err & [pth]]
  (update ctx :errors conj (if pth (str err " @ " (path-str pth)) err)))

(defn set-doc [ctx doc]
  (assoc ctx :doc doc))

(defmulti do-patch (fn [ctx {op :op :as p}] (keyword op)))
(defmulti validate (fn [ctx {op :op :as p}] (keyword op)))

(defmethod validate
  :test
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    (not (contains? op :value)) {:err (str "Missing value")}
    (path-errors (:path op)) {:err (str (path-errors (:path op))  " in path")}
    :else {:succ (-> op (update :path parse-path))}))

(defmethod do-patch :test
  [{obj :doc :as ctx} {pth :path v :value}]
  (let [vv (*get-in obj pth)]
    (if (= vv v)
      ctx
      (add-error ctx (str "Test failed. Expected " (to-json v) ", got " (to-json vv)) pth))))

(defmethod do-patch :add-if-null
  [{obj :doc :as ctx} {pth :path v :value}]
  (let [vv (*get-in obj pth)]
    (if (nil? vv)
      (set-doc ctx (*assoc-in obj pth v))
      ctx)))

(defn do-remove [obj k]
  (cond
    (map? obj)
    {:succ (let [kk (if (number? k) (keyword (str k)) k)]
             (dissoc obj kk))}

    (sequential? obj)
    (cond
      (not (integer? k))
      {:err (str "Should be index in array, got " k)}

      (not (<= k (count obj)))
      {:err (str "Out of Bounds")}

      :else
      (let [[before after] (split-at k obj)]
        {:succ (into (vec before) (rest after))}))

    :else
    {:err (str "Expected object or array, got " obj)}))


(defmethod validate
  :remove
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    :else {:succ (update op :path parse-path)}))

(defmethod validate
  :add-if-null
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    :else {:succ (update op :path parse-path)}))

(defmethod do-patch :remove
  [{obj :doc :as ctx} {pth :path v :value}]
  (let [ppath (butlast pth)
        k (last pth)
        parent (*get-in obj ppath)]
    (if (*contains? parent k)
      (if (= (count pth) 1)
        (let [{err :err succ :succ} (do-remove obj k)]
          (if err
            (add-error ctx err pth)
            (set-doc ctx succ)))
        (let [{err :err succ :succ} (do-remove parent k)]
          (if err
            (add-error ctx err pth)
            (set-doc ctx (*assoc-in obj ppath succ)))))
      (add-error ctx "Nothing to remove" pth))))

(defmethod validate
  :replace
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    (not (contains? op :value)) {:err (str "Missing value")}
    (path-errors (:path op)) {:err (str (path-errors (:path op))  " in path")}
    :else {:succ (update op :path parse-path)}))

(defmethod do-patch :replace
  [{obj :doc :as ctx} {pth :path v :value}]
  (if (empty? pth)
    (set-doc ctx v)
    (if-let [parent (*get-in obj (butlast pth))]
      (if (*contains? parent (last pth))
        (set-doc ctx (*assoc-in obj pth v))
        (add-error ctx "Value to replace does not present" pth))
      (add-error ctx "Value to replace does not present" pth))))

(defn do-add [obj k v]
  (cond
    (map? obj)
    {:succ (let [kk (if (number? k) (keyword (str k)) k)]
             (assoc obj kk v))}

    (sequential? obj)
    (cond
      (= k :jp/conj)
      {:succ (conj obj v)}

      (not (integer? k))
      {:err (str "Should be index in array, got " k)}

      (not (<= k (count obj)))
       {:err (str "Out of Bounds")}

      :else
      (let [[before after] (split-at k obj)]
        {:succ (into (into (vec before) [v]) after)}))

    :else
    {:err (str "Expected object or array, got " obj)}))


(defmethod validate :add
  [ctx op]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    (not (contains? op :value)) {:err (str "Missing value")}
    (path-errors (:path op)) {:err (str (path-errors (:path op))  " in path")}
    :else {:succ (update op :path parse-path)}))

(defmethod do-patch :add
  [{obj :doc :as ctx} {pth :path v :value :as op}]
  (if (empty? pth)  (set-doc ctx v)
      (let [ppth (butlast pth)
            last-k (last pth)
            parent (if (empty? ppth) obj (*get-in obj ppth))]
        (if-not parent
          (add-error ctx "No parent object or array" pth)
          (let [{err :err succ :succ} (do-add parent last-k v)]
            (if err
              (add-error ctx err pth)
              (set-doc ctx (if (empty? ppth) succ (*assoc-in obj ppth succ)))))))))


(defmethod validate
  :move
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    (not (:from op))  {:err (str "Missing from")}
    (path-errors (:path op)) {:err (str (path-errors (:path op))  " in path")}
    (path-errors (:from op)) {:err (str (path-errors (:from op))  " in path")}
    :else {:succ (-> op
                     (update :path parse-path)
                     (update :from parse-path))}))


(defmethod do-patch :move
  [{obj :doc :as ctx} {pth :path from :from v :value :as op}]
  (let [parent (*get-in obj (butlast from))
        v (get parent (last from))]
    (if (*contains? parent (last from))
      (-> ctx
       (do-patch (assoc op :op "remove" :value v :path from))
       (do-patch (assoc op :op "add" :value v)))
      (add-error ctx "Nothing to copy" from))))

(defmethod validate
  :copy
  [ctx {pth :path v :value :as op}]
  (cond
    (not (:path op))  {:err (str "Missing path")}
    (not (:from op))  {:err (str "Missing from")}
    (path-errors (:path op)) {:err (str (path-errors (:path op))  " in path")}
    (path-errors (:from op)) {:err (str (path-errors (:from op))  " in from")}
    :else {:succ (-> op
                     (update :path parse-path)
                     (update :from parse-path))}))

(defmethod do-patch :copy
  [{obj :doc :as ctx} {pth :path from :from v :value :as op}]
  (let [parent (*get-in obj (butlast from))
        v (get parent (last from))]
    (if (*contains? parent (last from))
      (do-patch ctx (assoc op :op "add" :value v))
      (add-error ctx "Nothing to copy" from))))

;; (cond-> p
;;   (:path p) (assoc :path (parse-path (:path p)))
;;   (:from p) (assoc :from (parse-path (:from p))))

(defmethod do-patch :default
  [{obj :doc :as ctx} op]
  {:errors (str "Unknown operation: " op)})

(defmethod validate
  :default
  [ctx op]
  {:err (str "Unknown operation")})

(defn json-patch [obj patches]
  (let [{obj :doc errs :errors} (reduce (fn [ctx p]
                                          (let [{err :err succ :succ} (validate {} p)]
                                            (if err
                                              (add-error ctx (str err " in " (cheshire.core/generate-string p)))
                                              (do-patch ctx succ))))
                                        {:doc obj} patches)]
    (if errs
      {:errors errs}
      {:success obj})))
