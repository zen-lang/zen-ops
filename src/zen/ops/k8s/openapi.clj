(ns zen.ops.k8s.openapi
  (:require
   [zen.core :as zen]
   [clojure.string :as str]
   [cheshire.core :as cheshire]
   [clojure.java.io :as io]
   [clojure.walk]))


(defmulti sch2zen (fn [x] (keyword (:type x))))


(defn normalize-ns [s]
  (-> s
      (str/replace #"core\.api\.k8s\.io\." "")
      (str/replace #"apps\.api\.k8s\.io\." "apps.")))

(defn build-schema-name [k]
  (let [k (if (keyword? k) (subs (str k) 1) k)
        [sym v & api] (reverse (str/split k  #"\."))]
    [(symbol (normalize-ns (str "k8s." (str/join "." api) "." v)))
     (symbol sym)]))

(defn k8s-name2zen [k]
  (apply symbol (mapv str (build-schema-name k))))


(defn ref2sym [ref]
  (-> ref
      (str/split #"/")
      last
      (k8s-name2zen)))

(defn *sch2zen [{desc :description ref :$ref
                 k8s-lmk :x-kubernetes-list-map-keys
                 k8s-lt :x-kubernetes-list-type
                 k8s-pmk :x-kubernetes-patch-merge-key
                 k8s-ps :x-kubernetes-patch-strategy
                 k8s-u :x-kubernetes-unions
                 k8s-gvk :x-kubernetes-group-version-kind
                 :as sch}]
  (-> (dissoc sch :description :$ref
              :x-kubernetes-list-map-keys
              :x-kubernetes-list-type
              :x-kubernetes-patch-merge-key
              :x-kubernetes-patch-strategy
              :x-kubernetes-group-version-kind
              :x-kubernetes-unions)
      (cond-> desc (assoc :zen/desc desc)
              ref (assoc :confirms #{(ref2sym ref)})
              k8s-lmk (assoc :k8s/list-map-keys k8s-lmk)
              k8s-lt (assoc :k8s/list-type k8s-lt)
              k8s-pmk (assoc :k8s/patch-merge-key k8s-pmk)
              k8s-ps (assoc :k8s/patch-strategy k8s-ps)
              k8s-u (assoc :k8s/unions k8s-u)
              k8s-gvk (assoc :k8s/api k8s-gvk)
              )
      (sch2zen)))

(defmethod sch2zen :default
  [sch]
  (if (not (:confirms sch))
    (assoc sch :type 'zen/any)
    sch))

(defmethod sch2zen :string
  [sch]
  (-> (assoc sch :type 'zen/string)
      (dissoc :format)))

(defmethod sch2zen :integer
  [sch]
  (-> 
   (assoc sch :type 'zen/integer)
   (dissoc :format)))

(defmethod sch2zen :boolean
  [sch]
  (assoc sch :type 'zen/boolean))

(defmethod sch2zen :number
  [sch]
  (assoc sch :type 'zen/number))

(defmethod sch2zen :object
  [{req :required props :properties aprops :additionalProperties :as sch}]

  (let [reqs (into  #{} (mapv keyword req))]
    (cond->
        (merge
          (dissoc sch :properties :additionalProperties :required)
          {:type 'zen/map})
      props
      (assoc :keys (reduce (fn [acc [k v]]
                        (assoc acc k (*sch2zen v)))
                      {} props))


      aprops
      (assoc :values (*sch2zen aprops))

      (seq reqs)
      (assoc :require reqs))))

(defmethod sch2zen :array
  [{its :items :as sch}]
  (merge (dissoc sch :items)
         {:type  'zen/vector
          :every (*sch2zen its)}))

(defn schema-to-zen [x]
  (merge (*sch2zen x)
         (cond-> {:zen/tags #{'zen/schema 'k8s/schema}
                  :type (get {"object" 'zen/map} (:type x) 'zen/any)}
           (:description x) (assoc :zen/desc (:description x)))))

(defn collect-imports [self n]
  (let [imp (atom #{})]
    (clojure.walk/postwalk
      (fn [x]
        (when-let [n (and (symbol? x) (namespace x))]
          (when-not (contains? #{"zen" (str self)} n)
            (swap! imp conj (symbol n))))
        x) n)
    @imp))


(defn load-schemas [ztx acc schemas]
  (->> schemas
       (reduce (fn [acc [k v]]
                 (let [[ns sym] (build-schema-name k)]
                   (-> acc
                       (update  ns merge {'ns ns})
                       (assoc-in [ns sym]
                                 (if (= sym 'JSONSchemaProps)
                                   {:zen/tags #{'zen/schema}
                                    :zen/desc "TODO: json-schema conv"
                                    :type 'zen/any}
                                   (schema-to-zen v))))))
               acc)))

(defn url-template [url]
  (->> (str/split url #"/")
       (filterv #(not (str/blank? %)))
       (mapv (fn [x]
               (if (and (str/starts-with? x "{")
                        (str/ends-with? x "}"))
                 (keyword (subs x 1 (dec (count x))))
                 x)))))
(defn process-params [params]
  (->> params
       (reduce (fn [acc {nm :name :as prm}]
                 (let [k (keyword nm)]
                   (-> acc (assoc k (cond-> (dissoc prm :name)
                                      (:schema prm) (assoc :schema (*sch2zen prm)))))))
               {})))

(defn build-ns-name [g v k]
  (str/join "." (->> ["k8s" g v k] (remove str/blank?))))


(defn split-camel [s]
  (->> 
   (str/split  (or s "") #"(?=\p{Upper})")
   (mapv str/lower-case)))

(defn normalize-op-id [s ks]
  (let [ks (into #{"core" "apps"} (mapcat split-camel ks))]
    (->> (split-camel (-> s
                          (str/replace #"ForAllNamespaces" "All")
                          (str/replace #"API" "Api")
                          (str/replace #"Namespaced" "")))
         (remove #(contains? ks %))
         (str/join "-")
         (symbol))))

(defn load-operations [ztx acc paths]
  (->> paths
       (reduce (fn [acc [url {prms :parameters :as ops}]]
                 (let [url (subs (str url) 1)]
                   (->> (dissoc ops :parameters)
                        (reduce (fn [acc [meth op]]
                                  (let [{g :group k :kind v :version} (:x-kubernetes-group-version-kind op)
                                        act (:x-kubernetes-action op)
                                        ns-name  (build-ns-name g v k)

                                        sym (when-let [oid (:operationId op)]
                                              (when-not (str/blank? oid)
                                                (normalize-op-id oid (into (str/split (or g "") #"\.") [v k]))))]
                                    (if-not sym
                                      acc
                                      (let [zop (-> (dissoc op :parameters)
                                                    (merge {:zen/tags #{'k8s/op}
                                                            :method   meth
                                                            :url      (url-template url)
                                                            :params   (process-params (concat (:parameters op) prms))}))]
                                        (-> (update acc (symbol ns-name) merge {'ns (symbol ns-name)})
                                            (update-in [(symbol ns-name) (symbol sym)]
                                                       (fn [x]
                                                         (when x (println :WARN :override ns-name sym))
                                                         zop)))))))
                                acc))))
               acc)))

(defn load-namespaces [ztx openapi]
  (let [nss (load-schemas ztx {} (:definitions openapi))]
    (load-operations ztx nss (:paths openapi))))


(defn *update-layer [nss k v]
  (let [w (get v :w 0)]
    (->> (get v 'import)
         (reduce (fn [nss dep]
                   (if (<= w (get-in nss [dep :w] 0))
                     (*update-layer (assoc-in nss [dep :w] (dec w))
                                    dep (assoc (get nss dep) :w (dec w)))
                     nss))
                 nss))))

(defn topologica-sort [nss]
  (->> nss
       (reduce (fn [nss [k v]] (*update-layer nss k v)) nss)
       (sort-by (fn [[_ {w :w}]] (or w 0)))))


(defn load-openapi [ztx openapi]
  (->> (load-namespaces ztx openapi)
       (reduce (fn [acc [k n]]
                 (assoc acc k (assoc n 'import (collect-imports k n))))
               {})
       (topologica-sort)
       (mapv (fn [[_ n]] (zen.core/load-ns ztx n)))))

(defn load-default-api [ztx]
  (let [k8s-swagger (cheshire.core/parse-string (slurp (io/resource "k8s-swagger.json")) keyword)]
    (zen/read-ns ztx 'k8s)
    (load-openapi ztx k8s-swagger)
    :loaded))

(defn build-filter [q]
  (when q (re-pattern (str ".*" (str/join ".*" (mapv str/lower-case (str/split q #"\s+"))) ".*"))))

(defn list-ops [ztx & [q]]
  (let [q (build-filter q)]
    (->> (cond->> (zen/get-tag ztx 'k8s/op)
           q (filterv (fn [x]
                        (not (nil? (re-matches q (str/lower-case (str x))))))))
         (sort))))

(defn list-schemas [ztx & [q]]
  (->>
    (cond->> (zen/get-tag ztx 'k8s/schema)
      q (filter (fn [x] (str/includes? (str/lower-case (str x)) q))))
    (sort)))

(defn op-def [ztx m]
  (let [m (if (map? m) (:method m) m)
        op-def (zen/get-symbol ztx (symbol m))]
    (when-not op-def (throw (Exception. (pr-str "Op not found " m))))
    op-def))

(defn build-request-params [ztx params-def params]
  (->> params-def
       (reduce (fn [acc [k pdef]]
                 (let [v (get params k)]
                   (if (and (:require pdef) (nil? v))
                     (update acc :errors (fn [x] (conj (or x []) (str "Missed parameter " k))))
                     (if v
                       (if-let [errs (and (:schema pdef)
                                          (let [{errs :errors} (zen.core/validate-schema ztx (:schema pdef) v)]
                                            (seq errs)))]
                         (update acc :errors (fn [x] (into (or x []) errs)))
                         (case (:in pdef)
                           "body"  (assoc acc :body (cheshire/generate-string v))
                           "query" (assoc-in acc [:query-params k] v)
                           "path"  acc))
                       acc))))
               {})))

(defn render-url [url-template params]
  (->> url-template
       (mapv (fn [x]
               (if (string? x)
                 x
                 (if-let [v (get params x)]
                   v
                   (throw (Exception. (pr-str :missed-param x params)))))))
       (str/join "/")))

(defn build-request [ztx {m :method params :params}]
  (let [opd (op-def ztx m)
        {errs :errors :as req} (build-request-params ztx (:params opd) params)]
    (when-not (empty? errs) (throw (Exception. (str/join "; " errs))))
    (let [url (render-url (:url opd) params)]
      (assoc req :url url :method (:method opd)))))


(defn validate [ztx res]
  (let [[g v] (str/split (:apiVersion res) #"/" 2)
        sym (symbol (build-ns-name g v nil) (:kind res))]
    (if (zen/get-symbol ztx sym)
      (assoc (zen/validate ztx #{sym} res) :zen/name sym)
      {:errors [{:message (str "Could not find schema for " sym)}]})))


(defn api-name [action res]
  (let [[g v] (str/split (:apiVersion res) #"/" 2)]
    (symbol (build-ns-name g v (:kind res)) action)))

(defn effective-schema [ztx sym]
  (clojure.walk/postwalk
   (fn [x]
     (if-let [cfrm  (and (map? x) (first (:confirms x)))]
       (merge x (effective-schema ztx cfrm))
       x))
   (zen/get-symbol ztx sym)))

(defn gen-sample [sch]
  (cond
    (= 'zen/map (:type sch))
    (->> (:keys sch)
         (reduce (fn [acc [k v]]
                   (assoc acc k (gen-sample v))
                   ) {}))

    (= 'zen/vector (:type sch))
    [(gen-sample (:every sch))]

    :else
    (or (:type sch) sch)))

(defn describe [ztx sym]
  (gen-sample (effective-schema ztx sym)))

(comment



  )
