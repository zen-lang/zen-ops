(ns json-patch.path
  (:require [clojure.string :as str]))

(defn unescape [p]
  (str/replace p #"(~0|~1)"
               (fn [[_ s]] (cond (= s "~0") "~"
                                 (= s "~1") "/"))))

(defn path-errors [path]
  (cond
    (not (or (str/starts-with? path "/") (= "" path)))
    (str "JSON Pointer should start with a slash. Path is " path)

    (re-matches #".*/0\d+(/.*|$)" path)
    (str "JSON Pointer has index with leading zeros. Path is " path)))

(defn parse-path [path]
  (when (string? path)
    (cond-> (->> (str/split path #"/")
                 rest
                 (into [])
                 (mapv (fn [x]
                         (cond
                           (re-matches #"^\d+" x) (Integer/parseInt x)
                           (= "-" x) :jp/conj
                           :else (keyword (unescape x))))))

      (str/ends-with? path "/") (into  [(keyword "")]))))

;; TODO: escapme path
(defn path-str [pth]
  (if (empty? pth)
    ""
    (->> pth
         (mapv #(if (keyword? %) (name %)  %))
         (str/join "/")
         (str "/"))))

(defn butlastv [xs]
  (loop [ret []
         [x & xs] xs]
    (if (empty? xs)
      (vec ret)
      (recur (conj ret x) xs))))
