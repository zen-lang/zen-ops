(ns zen.ops.k8s.openapi
  (:require
   [zen.core :as zen]
   [clojure.string :as str]
   [cheshire.core :as cheshire]
   [clojure.java.io :as io]
   [inflections.core :as infl]
   [klog.core :as klog]
   [clojure.walk]))

(defmulti sch2zen (fn [grp-idx x] (keyword (:type x))))

(defn build-groups-idx [api]
  (->>
   (:definitions api)
   (reduce
    (fn [acc [k v]]
      (let [{g :group} (first (:x-kubernetes-group-version-kind v))
            parts (str/split (subs (str k) 1) #"\.")
            n (str/join "." (drop-last 2 parts))]
        (if (nil? g)
          acc
          (update acc n (fn [x]
                          (when (and (not (nil? x))
                                     (not (= x g)))
                            (println :override x g))
                          g)))))
    {})))

(defn normalize-group [_g]
  (when _g
    (->
     (str/replace _g #"^io\.k8s\.apimachinery\.pkg" "k8s.v1"))))

(defn translate-ns [grps-map k]
  (let [parts (str/split (if (keyword? k) (subs (str k) 1) k) #"\.")
        [_k _v & _] (reverse parts)
        _g (str/join "." (into [] (drop-last 2 parts)))]
    (if-let [g (get grps-map _g)]
      [(symbol (->
                (str "k8s." (if (str/blank? g) "" (str g ".")) _v )
                (normalize-group)))
       (symbol _k)]
      [(symbol (normalize-group _g))
       (symbol _k)])))

(defn ref2sym [grp-idx ref]
  (let [sym (-> ref (str/split #"/") last)]
    (apply symbol (mapv str (translate-ns grp-idx sym)))))

(defn *sch2zen [grp-idx {desc :description ref :$ref
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
              ref (assoc :confirms #{(ref2sym grp-idx ref)})
              k8s-lmk (assoc :k8s/list-map-keys k8s-lmk)
              k8s-lt (assoc :k8s/list-type k8s-lt)
              k8s-pmk (assoc :k8s/patch-merge-key k8s-pmk)
              k8s-ps (assoc :k8s/patch-strategy k8s-ps)
              k8s-u (assoc :k8s/unions k8s-u)
              k8s-gvk (assoc :k8s/api k8s-gvk :zen/tags #{'k8s/resource})
              )
      (->> (sch2zen grp-idx ))))

(defmethod sch2zen :default
  [grp-idx sch]
  (if (not (:confirms sch))
    (assoc sch :type 'zen/any)
    sch))

(defmethod sch2zen :string
  [grp-idx sch]
  (-> (assoc sch :type 'zen/string)
      (dissoc :format)))

(defmethod sch2zen :integer
  [grp-idx sch]
  (-> 
   (assoc sch :type 'zen/integer)
   (dissoc :format)))

(defmethod sch2zen :boolean
  [grp-idx sch]
  (assoc sch :type 'zen/boolean))

(defmethod sch2zen :number
  [grp-idx sch]
  (assoc sch :type 'zen/number))

(defmethod sch2zen :object
  [grp-idx {req :required props :properties aprops :additionalProperties :as sch}]

  (let [reqs (into  #{} (mapv keyword req))]
    (cond->
        (merge
         (dissoc sch :properties :additionalProperties :required)
         {:type 'zen/map})
      props
      (assoc :keys (reduce (fn [acc [k v]]
                             (assoc acc k (*sch2zen grp-idx v)))
                           {} props))


      aprops
      (assoc :values (*sch2zen grp-idx aprops))

      (seq reqs)
      (assoc :require reqs))))

(defmethod sch2zen :array
  [grp-idx {its :items :as sch}]
  (merge (dissoc sch :items)
         {:type  'zen/vector
          :every (*sch2zen grp-idx its)}))

(into #{:a} #{:b})

(defn schema-to-zen [grp-idx x]
  (let [zsch (*sch2zen grp-idx x)]
    (merge  zsch
            (cond-> {:zen/tags (cond-> #{'zen/schema 'k8s/schema}
                                 (:zen/tags zsch) (into (:zen/tags zsch)))
                     :type (get {"object" 'zen/map} (:type x) 'zen/any)}
              (:description x) (assoc :zen/desc (:description x))))))

(defn collect-imports [self n]
  (let [imp (atom #{})]
    (clojure.walk/postwalk
     (fn [x]
       (when-let [n (and (symbol? x) (namespace x))]
         (when-not (contains? #{"zen" (str self)} n)
           (swap! imp conj (symbol n))))
       x) n)
    @imp))


(defn load-schemas [ztx grp-idx acc schemas]
  (->> schemas
       (reduce (fn [acc [k v]]
                 (let [[ns sym] (translate-ns grp-idx k)]
                   (-> acc
                       (update  ns merge {'ns ns})
                       (assoc-in [ns sym]
                                 (if (= sym 'JSONSchemaProps)
                                   {:zen/tags #{'zen/schema}
                                    :zen/desc "TODO: json-schema conv"
                                    :type 'zen/any}
                                   (schema-to-zen grp-idx v))))))
               acc)))

(defn url-template [url]
  (->> (str/split url #"/")
       (filterv #(not (str/blank? %)))
       (mapv (fn [x]
               (if (and (str/starts-with? x "{")
                        (str/ends-with? x "}"))
                 (keyword (subs x 1 (dec (count x))))
                 x)))))

(defn process-params [grp-idx params]
  (->> params
       (reduce (fn [acc {nm :name :as prm}]
                 (let [k (keyword nm)]
                   (-> acc
                       (assoc-in [:keys  k]
                                 (-> (*sch2zen grp-idx (cond-> (:schema prm)
                                                         (:type prm) (assoc :type (:type prm))))
                                     (assoc :zen/desc (:description prm)
                                            :openapi/in (:in prm))
                                     (cond->
                                         (:uniqueItems prm) (assoc :k8s/uniqueItems true))))
                       (cond->
                           (:required prm) (update :require conj k)))))
               {:type 'zen/map
                :require #{}
                :keys {}})))

(defn build-ns-name [g v k]
  (str/join "." (->> ["k8s" (normalize-group g) v k] (remove str/blank?))))


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

(defn load-operations [ztx grp-idx acc paths]
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
                                      (let [zop (-> (dissoc op :parameters :description
                                                            :x-kubernetes-action
                                                            :x-kubernetes-group-version-kind :operationId :response)
                                                    (merge {:zen/tags #{'k8s/op}
                                                            :zen/desc (:description op)
                                                            :k8s/api (:x-kubernetes-group-version-kind op) 
                                                            :k8s/oid (:operationId op)
                                                            :k8s/action (:x-kubernetes-action op)
                                                            :openapi/method  meth
                                                            :openapi/url      (url-template url)
                                                            :params   (process-params grp-idx (concat (:parameters op) prms))
                                                            :result (when-let [s (get-in op [:responses (keyword "200") :schema])]
                                                                      s #_(*sch2zen grp-idx s))}))]
                                        (-> (update acc (symbol ns-name) merge {'ns (symbol ns-name)})
                                            (update-in [(symbol ns-name) (symbol sym)]
                                                       (fn [x]
                                                         (when x (klog/log :k8s/override {:ns ns-name :sym  sym}))
                                                         zop)))))))
                                acc))))
               acc)))

(defn load-namespaces [ztx openapi]
  (let [grp-idx (build-groups-idx openapi)
        nss (load-schemas ztx grp-idx {} (:definitions openapi))]
    (load-operations ztx grp-idx nss (:paths openapi))))


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

(defn ilike-filter [q xs]
  (let [q (build-filter q)]
    (->> (cond->> xs
           q (filterv (fn [x]
                        (not (nil? (re-matches q (str/lower-case (str x))))))))
         (sort))))

(defn list-ops [ztx & [q]]
  (->> (zen/get-tag ztx 'k8s/op)
       (ilike-filter q)))

(defn list-schemas [ztx & [q]]
  (->> (zen/get-tag ztx 'k8s/schema)
       (ilike-filter q)))

(defn list-resources [ztx & [q]]
  (->> (zen/get-tag ztx 'k8s/resource)
       (ilike-filter q)))

(defn op-def [ztx m]
  (let [m (if (map? m) (:method m) m)
        op-def (zen/get-symbol ztx (symbol m))]
    (when-not op-def (throw (Exception. (pr-str "Op not found " m))))
    op-def))

(defn build-request-params [ztx params-def params]
  (->> (:keys params-def)
       (reduce (fn [acc [k pdef]]
                 (let [v (get params k)]
                   (if v
                     (case (:openapi/in pdef)
                       "body"  (assoc acc :body (cheshire/generate-string v))
                       "query" (assoc-in acc [:query-params k] v)
                       "path"  acc)
                     acc)))
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

(defn *build-request [ztx opd {m :method params :params}]
  (let [{errs :errors } (zen/validate-schema ztx (:params opd) params)]
    (if-not (empty? errs)
      {:error {:errors errs :params params}
       :schema (:params opd)}
      (let [req (build-request-params ztx (:params opd) params)
            url (render-url (:openapi/url opd) params)]
        (cond-> (assoc req :url url :method (:openapi/method opd))
          (:openapi/content-type opd)
          (assoc-in [:headers "content-type"] (:openapi/content-type opd)))))))

(defn build-request [ztx {m :method :as req}]
  (*build-request ztx (op-def ztx m) req))


(defn validate [ztx res]
  (if-let [tp (:k8s/type res)]
    (assoc (zen/validate ztx #{tp} (dissoc res :k8s/type)) :zen/name tp)
    (let [[g v] (str/split (:apiVersion res) #"/" 2)
          sym (symbol (build-ns-name g v nil) (:kind res))]
      (if (zen/get-symbol ztx sym)
        (assoc (zen/validate ztx #{sym} res) :zen/name sym)
        {:errors [{:message (str "Could not find schema for " sym)}]}))))


(defn api-name [action res]
  (if-let [tp (:k8s/type res)]
    (symbol (str (namespace tp) "." (name tp)) action)
    (let [[g v] (str/split (:apiVersion res) #"/" 2)]
      (symbol (build-ns-name g v (:kind res)) action))))

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

(defn plural [s]
  (when s (infl/plural (str/lower-case s))))

(def list-params 
  {:type 'zen/map
   :require #{}
   :keys
   {:allowWatchBookmarks
    {:type 'zen/boolean
     :zen/desc
     "allowWatchBookmarks requests watch events with type \"BOOKMARK\". Servers that do not implement bookmarks may ignore this flag and bookmarks are sent at the server's discretion. Clients should not assume bookmarks are returned at any specific interval, nor may they assume the server will send any BOOKMARK event during a session. If this is not a watch, this field is ignored. If the feature gate WatchBookmarks is not enabled in apiserver, this field is ignored."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :limit
    {:type 'zen/integer
     :zen/desc
     "limit is a maximum number of responses to return for a list call. If more items exist, the server will set the `continue` field on the list metadata to a value that can be used with the same initial query to retrieve the next set of results. Setting a limit may return fewer than the requested amount of items (up to zero items) in the event all requested objects are filtered out and clients should only use the presence of the continue field to determine whether more results are available. Servers may choose not to support the limit argument and will return all of the available results. If limit is specified and the continue field is empty, clients may assume that no more results are available. This field is not supported if watch is true.\n\nThe server guarantees that the objects returned when using continue will be identical to issuing a single list call without a limit - that is, no objects created, modified, or deleted after the first request is issued will be included in any subsequent continued requests. This is sometimes referred to as a consistent snapshot, and ensures that a client that is using limit to receive smaller chunks of a very large result can ensure they see all possible objects. If objects are updated during a chunked list the version of the object that was present at the time the first list result was calculated is returned."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :continue
    {:type 'zen/string
     :zen/desc
     "The continue option should be set when retrieving more results from the server. Since this value is server defined, clients may only use the continue value from a previous query result with identical query parameters (except for the value of continue) and the server may reject a continue value it does not recognize. If the specified continue value is no longer valid whether due to expiration (generally five to fifteen minutes) or a configuration change on the server, the server will respond with a 410 ResourceExpired error together with a continue token. If the client needs a consistent list, it must restart their list without the continue field. Otherwise, the client may send another list request with the token received with the 410 error, the server will respond with a list starting from the next key, but from the latest snapshot, which is inconsistent from the previous list results - objects that are created, modified, or deleted after the first list request will be included in the response, as long as their keys are after the \"next key\".\n\nThis field is not supported when watch is true. Clients may start a watch from the last resourceVersion value returned by the server and not miss any modifications."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :resourceVersion
    {:type 'zen/string
     :zen/desc
     "resourceVersion sets a constraint on what resource versions a request may be served from. See https://kubernetes.io/docs/reference/using-api/api-concepts/#resource-versions for details.\n\nDefaults to unset"
     :openapi/in "query"
     :k8s/uniqueItems true}
    :labelSelector
    {:type 'zen/string
     :zen/desc
     "A selector to restrict the list of returned objects by their labels. Defaults to everything."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :timeoutSeconds
    {:type 'zen/integer
     :zen/desc
     "Timeout for the list/watch call. This limits the duration of the call, regardless of any activity or inactivity."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :resourceVersionMatch
    {:type 'zen/string
     :zen/desc
     "resourceVersionMatch determines how resourceVersion is applied to list calls. It is highly recommended that resourceVersionMatch be set for list calls where resourceVersion is set See https://kubernetes.io/docs/reference/using-api/api-concepts/#resource-versions for details.\n\nDefaults to unset"
     :openapi/in "query"
     :k8s/uniqueItems true}
    :watch
    {:type 'zen/boolean
     :zen/desc
     "Watch for changes to the described resources and return them as a stream of add, update, and remove notifications. Specify resourceVersion."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :pretty
    {:type 'zen/string
     :zen/desc "If 'true', then the output is pretty printed."
     :openapi/in "query"
     :k8s/uniqueItems true}
    :fieldSelector
    {:type 'zen/string
     :zen/desc
     "A selector to restrict the list of returned objects by their fields. Defaults to everything."
     :openapi/in "query"
     :k8s/uniqueItems true}}})

(defn gen-gvk [res]
  (if-let [tp (:k8s/type res)]
    (let [parts (str/split (namespace tp) #"\.")
          g (->> parts (drop 1) (drop-last 1) (str/join "."))
          k (name tp)
          v (last parts)]
      [g v k])
    (throw (Exception. (str "No :k8s/type" res)))))

(defn gen-list-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :get
     :openapi/url (if (str/blank? g)
                    ["api" v "namespaces" :namespace (plural k)]
                    ["apis" g v "namespaces" :namespace (plural k)])
     :params
     (->
      (assoc-in list-params 
                [:keys :namespace]
                {:type 'zen/string,
                 :zen/desc "object name and auth scope, such as for teams and projects",
                 :openapi/in "path",
                 :k8s/uniqueItems true})
      (update :require conj :namespace))}))


(defn gen-list-all-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :get
     :openapi/url (if (str/blank? g)
                    ["api" v  (plural k)]
                    ["apis" g v  (plural k)])
     :params list-params}))

(def read-params
  {:type 'zen/map,
   :require #{:name},
   :keys
   {:exact
    {:type 'zen/boolean,
     :zen/desc
     "Should the export be exact.  Exact export maintains cluster-specific fields like 'Namespace'. Deprecated. Planned for removal in 1.18.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :export
    {:type 'zen/boolean,
     :zen/desc
     "Should this value be exported.  Export strips fields that a user can not specify. Deprecated. Planned for removal in 1.18.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :name
    {:type 'zen/string,
     :zen/desc "name of the Deployment",
     :openapi/in "path",
     :k8s/uniqueItems true},
    :pretty
    {:type 'zen/string,
     :zen/desc "If 'true', then the output is pretty printed.",
     :openapi/in "query",
     :k8s/uniqueItems true}}})


(defn gen-read-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :get
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name])
     :params (-> (assoc-in read-params
                        [:keys :namespace]
                        {:type 'zen/string,
                         :zen/desc "object name and auth scope, such as for teams and projects",
                         :openapi/in "path",
                         :k8s/uniqueItems true})
                 (update :require conj :namespace))}))

(defn gen-read-all-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :get
     :openapi/url (if (str/blank? g)
                    ["api" v  (plural k) :name]
                    ["apis" g v  (plural k) :name])
     :params read-params}))

(def delete-params
  {:type 'zen/map,
   :require #{:name},
   :keys
   {:body {:confirms #{'k8s.v1/DeleteOptions}, :openapi/in "body"},
    :dryRun
    {:type 'zen/string,
     :zen/desc "When present, indicates that modifications should not be persisted. An invalid or unrecognized dryRun directive will result in an error response and no further processing of the request. Valid values are: - All: all dry run stages will be processed",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :gracePeriodSeconds
    {:type 'zen/integer,
     :zen/desc "The duration in seconds before the object should be deleted. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period for the specified type will be used. Defaults to a per object value if not specified. zero means delete immediately.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :orphanDependents
    {:type 'zen/boolean,
     :zen/desc "Deprecated: please use the PropagationPolicy, this field will be deprecated in 1.7. Should the dependent objects be orphaned. If true/false, the \"orphan\" finalizer will be added to/removed from the object's finalizers list. Either this field or PropagationPolicy may be set, but not both.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :propagationPolicy
    {:type 'zen/string,
     :zen/desc "Whether and how garbage collection will be performed. Either this field or OrphanDependents may be set, but not both. The default policy is decided by the existing finalizer set in the metadata.finalizers and the resource-specific default policy. Acceptable values are: 'Orphan' - orphan the dependents; 'Background' - allow the garbage collector to delete the dependents in the background; 'Foreground' - a cascading policy that deletes all dependents in the foreground.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :name
    {:type 'zen/string,
     :zen/desc "name of the CustomResourceDefinition",
     :openapi/in "path",
     :k8s/uniqueItems true},
    :pretty
    {:type 'zen/string,
     :zen/desc "If 'true', then the output is pretty printed.",
     :openapi/in "query",
     :k8s/uniqueItems true}}})

(defn gen-delete-all-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :delete
     :openapi/url (if (str/blank? g)
                    ["api" v  (plural k) :name]
                    ["apis" g v  (plural k) :name])
     :params delete-params}))

(defn gen-delete-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :delete
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name])
     :params (assoc-in delete-params
                       [:keys :namespace]
                       {:type 'zen/string,
                        :zen/desc "object name and auth scope, such as for teams and projects",
                        :openapi/in "path",
                        :k8s/uniqueItems true})}))


(def create-params
  {:type 'zen/map,
   :require #{:body},
   :keys
   {:body {:openapi/in "body"},
    :dryRun
    {:type 'zen/string,
     :zen/desc "When present, indicates that modifications should not be persisted. An invalid or unrecognized dryRun directive will result in an error response and no further processing of the request. Valid values are: - All: all dry run stages will be processed",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :fieldManager
    {:type 'zen/string,
     :zen/desc "fieldManager is a name associated with the actor or entity that is making these changes. The value must be less than or 128 characters long, and only contain printable characters, as defined by https://golang.org/pkg/unicode/#IsPrint.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :pretty
    {:type 'zen/string,
     :zen/desc "If 'true', then the output is pretty printed.",
     :openapi/in "query",
     :k8s/uniqueItems true}}})

(defn gen-create-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :post
     :k8s/api {:group g :kind k :version v},
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k)]
                    ["apis" g v  "namespaces" :namespace  (plural k)])
     :params (-> (assoc-in create-params [:keys :body :confirms] #{(:k8s/type res)})
                 (update :require conj :namespace)
                 (assoc-in [:keys :namespace]
                           {:type 'zen/string,
                            :zen/desc "object name and auth scope, such as for teams and projects",
                            :openapi/in "path",
                            :k8s/uniqueItems true}))}))

(defn gen-create-all-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :post
     :k8s/api {:group g :kind k :version v},
     :openapi/url (if (str/blank? g)
                    ["api" v   (plural k)]
                    ["apis" g v   (plural k)])
     :params (assoc-in create-params [:keys :body :confirms] #{(:k8s/type res)})}))

(def replace-params {:type 'zen/map,
                     :require #{:name :body},
                     :keys
                     {:body {:openapi/in "body"},
                      :dryRun {:type 'zen/string,
                               :zen/desc "When present, indicates that modifications should not be persisted. An invalid or unrecognized dryRun directive will result in an error response and no further processing of the request. Valid values are: - All: all dry run stages will be processed",
                               :openapi/in "query",
                               :k8s/uniqueItems true},
                      :fieldManager
                      {:type 'zen/string,
                       :zen/desc "fieldManager is a name associated with the actor or entity that is making these changes. The value must be less than or 128 characters long, and only contain printable characters, as defined by https://golang.org/pkg/unicode/#IsPrint.",
                       :openapi/in "query",
                       :k8s/uniqueItems true},
                      :name
                      {:type 'zen/string,
                       :zen/desc "name of the Deployment",
                       :openapi/in "path",
                       :k8s/uniqueItems true},
                      :pretty
                      {:type 'zen/string,
                       :zen/desc "If 'true', then the output is pretty printed.",
                       :openapi/in "query",
                       :k8s/uniqueItems true}}})


(defn gen-replace-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :put
     :k8s/api {:group g :kind k :version v}
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name])
     :params (-> (assoc-in replace-params [:keys :body :confirms] #{(:k8s/type res)})
                 (update :require conj :namespace)
                 (assoc-in [:keys :namespace]
                           {:type 'zen/string
                            :zen/desc "object name and auth scope, such as for teams and projects"
                            :openapi/in "path"
                            :k8s/uniqueItems true}))}))

(def patch-params 
  {:type 'zen/map,
   :require #{:name :body :namespace},
   :keys
   {:body {:confirms #{'k8s.v1/Patch} :openapi/in "body"},
    :dryRun
    {:type 'zen/string,
     :zen/desc
     "When present, indicates that modifications should not be persisted. An invalid or unrecognized dryRun directive will result in an error response and no further processing of the request. Valid values are: - All: all dry run stages will be processed",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :fieldManager
    {:type 'zen/string,
     :zen/desc
     "fieldManager is a name associated with the actor or entity that is making these changes. The value must be less than or 128 characters long, and only contain printable characters, as defined by https://golang.org/pkg/unicode/#IsPrint. This field is required for apply requests (application/apply-patch) but optional for non-apply patch types (JsonPatch, MergePatch, StrategicMergePatch).",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :force
    {:type 'zen/boolean,
     :zen/desc
     "Force is going to \"force\" Apply requests. It means user will re-acquire conflicting fields owned by other people. Force flag must be unset for non-apply patch requests.",
     :openapi/in "query",
     :k8s/uniqueItems true},
    :name
    {:type 'zen/string,
     :zen/desc "name of the Service",
     :openapi/in "path",
     :k8s/uniqueItems true},
    :namespace
    {:type 'zen/string,
     :zen/desc "object name and auth scope, such as for teams and projects",
     :openapi/in "path",
     :k8s/uniqueItems true},
    :pretty
    {:type 'zen/string,
     :zen/desc "If 'true', then the output is pretty printed.",
     :openapi/in "query",
     :k8s/uniqueItems true}}})

(defn gen-patch-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :patch
     :k8s/api {:group g :kind k :version v}
     :openapi/content-type "application/merge-patch+json"
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name])
     :params (-> (assoc-in patch-params [:keys :body] {:type 'zen/any :openapi/in "body"})
                 (update :require conj :namespace)
                 (assoc-in [:keys :namespace]
                           {:type 'zen/string
                            :zen/desc "object name and auth scope, such as for teams and projects"
                            :openapi/in "path"
                            :k8s/uniqueItems true}))}))


(defn gen-replace-all-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :put
     :k8s/api {:group g :kind k :version v}
     :openapi/url (if (str/blank? g)
                    ["api" v  (plural k) :name]
                    ["apis" g v  (plural k) :name])
     :params (assoc-in replace-params [:keys :body :confirms] #{(:k8s/type res)})}))


(defn gen-replace-status-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :put
     :k8s/api {:group g :kind k :version v}
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name "status"]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name "status"])
     :params (-> (assoc-in replace-params [:keys :body :confirms] #{(:k8s/type res)})
                 (update :require conj :namespace)
                 (assoc-in [:keys :namespace]
                           {:type 'zen/string
                            :zen/desc "object name and auth scope, such as for teams and projects"
                            :openapi/in "path"
                            :k8s/uniqueItems true}))}))

(defn gen-patch-status-def [ztx res]
  (let [[g v k] (gen-gvk res)]
    {:openapi/method :patch
     :k8s/api {:group g :kind k :version v}
     :openapi/content-type "application/merge-patch+json"
     :openapi/url (if (str/blank? g)
                    ["api" v  "namespaces" :namespace  (plural k) :name "status"]
                    ["apis" g v  "namespaces" :namespace  (plural k) :name "status"])
     :params (-> (assoc-in replace-params [:keys :body] {:openapi/in "body" :type 'zen/any})
                 (update :require conj :namespace)
                 (assoc-in [:keys :namespace]
                           {:type 'zen/string
                            :zen/desc "object name and auth scope, such as for teams and projects"
                            :openapi/in "path"
                            :k8s/uniqueItems true}))}))

(defn describe [ztx sym]
  (gen-sample (effective-schema ztx sym)))

(comment



  )
