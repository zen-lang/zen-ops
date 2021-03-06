(ns zen.ops.k8s.core
  (:require
   [zen.core :as zen]
   [zen.ops.k8s.openapi :as openapi]
   [zen.ops.k8s.exec]
   [org.httpkit.client :as http]
   [matcho.core :as matcho]
   [klog.core :as klog]
   [cheshire.core :as cheshire]))


(def build-request openapi/build-request)


(defn init-context [ztx opts]
  (openapi/load-default-api ztx))

(def list-ops openapi/list-ops)

(def list-schemas openapi/list-schemas)

(def op-def openapi/op-def)

(def validate openapi/validate)
(def describe openapi/describe)

(defn request [ztx conn req]
  (let [{u :url t :token} conn]
    (-> @(http/request (merge req {:url (str u "/" (:url req))
                                   :headers (cond-> (or (:headers req) {})
                                              t (assoc "Authorization" (str "Bearer " t)))}))
        (update :body (fn [x]
                        (when x (cheshire/parse-string x keyword)))))))

(defn load-cluster-api [ztx conn]
  (let [openapi-resp (request ztx conn {:method :get :url "openapi/v2"})]
    (if (= 200 (:status openapi-resp))
      (do
        (openapi/load-openapi ztx (:body openapi-resp))
        ztx)
      (throw (Exception. (pr-str openapi-resp))))
    (:body openapi-resp)))


(defn op [ztx conn req]
  (let [res (request ztx conn (build-request ztx req))]
    (if (and (:status res) (< (:status res) 300))
      {:result (-> res :body)}
      {:error (or (:body res) res)})))

(defn *do-request [ztx conn op-def res]
  (let [req (openapi/*build-request ztx op-def (-> (dissoc res :k8s/type)
                                                   (update :params (fn [x] (or x {})))))]
    (if (:error req)
      req
      (let [res (request ztx conn req)]
        (if (and (:status res) (< (:status res) 300))
          {:result (-> res :body)
           :request req}
          {:error (or (:body res) res)
           :request req})))))

(defn do-list [ztx conn res]
  (*do-request ztx conn (openapi/gen-list-def ztx res) res))

(defn do-list-all [ztx conn res]
  (*do-request ztx conn (openapi/gen-list-all-def ztx res) res))

(defn do-read [ztx conn res & [params]]
  (*do-request ztx conn (openapi/gen-read-def ztx res)
               {:params (merge params (select-keys (:metadata res) [:name :namespace]))}))

(defn do-read-all [ztx conn res & [params]]
  (*do-request ztx conn (openapi/gen-read-all-def ztx res)
               {:params (merge params (:metadata res))}))


(defn enrich-with-kind [api res]
  (merge {:kind (:kind api) :apiVersion (str (:group api) "/" (:version api))}
         (dissoc res :k8s/type)))

(defn do-create [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-create-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:namespace (get-in res [:metadata :namespace])
                                  :body (enrich-with-kind api res)})})))

(defn do-create-all [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-create-all-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:body (enrich-with-kind api res)})})))

(defn do-replace [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-replace-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:namespace (get-in res [:metadata :namespace])
                                  :name (get-in res [:metadata :name])
                                  :body (enrich-with-kind api res)})})))

(defn do-patch [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-patch-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:namespace (get-in res [:metadata :namespace])
                                  :name (get-in res [:metadata :name])
                                  :body (enrich-with-kind api res)})})))

(defn do-delete-all [ztx conn res & [params]]
  (*do-request ztx conn (openapi/gen-delete-all-def ztx res)
               {:params (merge params {:name (get-in res [:metadata :name])})}))

(defn do-delete [ztx conn res & [params]]
  (*do-request ztx conn (openapi/gen-delete-def ztx res)
               {:params (merge params {:name (get-in res [:metadata :name])
                                       :namespace (get-in res [:metadata :namespace])})}))

(defn do-replace-all [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-replace-all-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:name (get-in res [:metadata :name])
                                  :body (enrich-with-kind api res)})})))

(defn do-replace-status [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-replace-status-def ztx res)
        {old :result err :error :as resp} (do-read ztx conn res)]
    (if err
      resp
      (*do-request ztx conn opd
                   {:params (merge params
                                   {:namespace (get-in res [:metadata :namespace])
                                    :name (get-in res [:metadata :name])
                                    :body (-> (enrich-with-kind api res)
                                              (assoc-in [:metadata :resourceVersion]
                                                        (get-in old [:metadata :resourceVersion])))})}))))

(defn do-patch-status [ztx conn res & [params]]
  (let [{api :k8s/api :as opd} (openapi/gen-patch-status-def ztx res)]
    (*do-request ztx conn opd
                 {:params (merge params
                                 {:namespace (get-in res [:metadata :namespace])
                                  :name (get-in res [:metadata :name])
                                  :body (-> (enrich-with-kind api res))})})))

(defn items
  ([ks res]
   (if-let [err (:error res)]
     err
     (->> res
          (:result)
          (:items)
          (mapv (fn [x]
                  (into [(get-in x [:metadata :name])]
                        (->> ks (mapv #(get-in x %)))))))))
  ([res] (items [] res)))


(def exec zen.ops.k8s.exec/exec)

(defn print-error [res]
  (when-let [err (:error res)]
    (klog/log :k8s/error {:err (:error res)}))
  res)

(defn hide-no-update [{no-update :k8s/no-update :as res} old-res]
  (if (nil? no-update)
    res
    (->> no-update
         (reduce (fn [res pth]
                   (cond
                     (nil? (get-in old-res pth)) res
                     (nil? (get-in res pth)) res

                     (get-in res pth) (do
                                        ;; (klog/log ::hide {:pth pth :new (get-in res pth) :old (get-in old-res pth)})
                                        (update-in res (butlast pth) dissoc (last pth)))
                     :else res))
                 res))))

(defn do-apply-ns  [ktx conn resource]
  (let [metadata (select-keys (:metadata resource) [:name :namespace])
        {err :error old-resource :result :as resp} (do-read ktx conn resource)]
    (print-error
     (if (= 404 (:code err))
       (->
        (do-create ktx conn (dissoc resource :k8s/no-update))
        (assoc :action :create))
       (let [for-update (hide-no-update resource old-resource)
             diff (matcho/match* old-resource (dissoc for-update :k8s/type :k8s/no-update))]
         (if (seq diff)
           (do
             (klog/log :k8s.apply/diff {:k8s/type (:k8s/type resource)
                                        :name (str (:namespace metadata) "/" (:name metadata))
                                        :diff diff})
             (-> (do-patch ktx conn for-update)
                 (assoc :action :patch)))
           (assoc resp :action :unchanged)))))))

(defn do-apply-all [ktx conn resource]
  (let [metadata (select-keys (:metadata resource) [:name])
        {err :error old-resource :result :as resp} (do-read-all ktx conn resource)]
    (print-error
     (if (= 404 (:code err))
       (->
        (do-create-all ktx conn resource)
        (assoc :action :create))
       (let [diff (matcho/match*  old-resource (dissoc resource :k8s/type))]
         (if (seq diff)
           (do (klog/log :k8s.apply/diff {:diff diff})
               (->
                (do-replace-all
                 ktx conn
                 (cond-> resource
                   (get-in old-resource [:metadata :resourceVersion])
                   (assoc-in [:metadata :resourceVersion]
                             (get-in old-resource [:metadata :resourceVersion]))))
                (assoc :action :replace)))
           (assoc resp :action :unchanged)))))))

(defn do-apply [ktx conn resource]
  (if (sequential? resource)
    (mapv (fn [res] (do-apply ktx conn res)) resource)
    (if (get-in resource [:metadata :namespace])
      (do-apply-ns ktx conn resource)
      (do-apply-all ktx conn resource))))


(comment

  (def conn {:url "http://localhost:8080"})

  (def ztx (zen/new-context {}))
  (init-context ztx {})

  (list-ops ztx "depl")

  (list-schemas ztx "ingres")

  (zen/get-symbol ztx 'k8s.networking.k8s.io.v1/Ingress)

  (describe ztx 'k8s.networking.k8s.io.v1/Ingress)

  (->>
   (op ztx conn {:method 'k8s.apps.v1.Deployment/list-all})
   #_(items))

  (list-schemas ztx "namespac")

  (do-apply ztx conn
            {:k8s/type 'k8s.v1/Namespace
             :metadata {:name "myns"
                        :labels {:managedBy "zenops"}}})

  (->>
   (op ztx conn {:method 'k8s.v1.Namespace/list})
   (items))




  )
