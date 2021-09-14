(ns zen.ops.k8s.core
  (:require
   [zen.core :as zen]
   [zen.ops.k8s.openapi :as openapi]
   [zen.ops.k8s.exec]
   [org.httpkit.client :as http]
   [matcho.core :as matcho]
   [cheshire.core :as cheshire]))


(def build-request openapi/build-request)
(defn request [ztx req]
  (let [{u :kube/url t :kube/token} (:kube/config @ztx)]
    (-> @(http/request (merge req {:url (str u "/" (:url req))
                                   :headers (cond-> {}
                                              t (assoc "Authorization" (str "Bearer " t)))}))
        (update :body (fn [x]
                        (when x (cheshire/parse-string x keyword)))))))

;; TODO: replace with init
(defn new-context [config]
  (let [ztx (zen.core/new-context {:kube/config config})]
    (openapi/load-default-api ztx)
    ztx))

(defn load-cluster-api [ztx]
  (let [config (:kube/config @ztx)
        openapi-resp (request ztx {:method :get :url "openapi/v2"
                                   :headers {"authorization" (str "Bearer " (:kube/token config))}})]
    (if (= 200 (:status openapi-resp))
      (do
        (openapi/load-openapi ztx (:body openapi-resp))
        ztx)
      (throw (Exception. (pr-str openapi-resp))))))


(def list-ops openapi/list-ops)

(def list-schemas openapi/list-schemas)

(def op-def openapi/op-def)

(def validate openapi/validate)

(defn op [ztx req]
  (let [res (request ztx (build-request ztx req))]
    (if (and (:status res) (< (:status res) 300))
      {:result (-> res :body)}
      {:error (or (:body res) res)})))

(defn items
  ([ks res]
   (->> res
        (:result)
        (:items)
        (mapv (fn [x]
                (into [(get-in x [:metadata :name])]
                      (->> ks (mapv #(get-in x %))))))))
  ([res] (items [] res)))


(def exec zen.ops.k8s.exec/exec)

(defn print-error [res]
  (when-let [err (:error res)]
    (println "ERROR:" (:error err)))
  res)

;; TODO: rename to apply
(defn update-resource [ktx resource]
  (let [metadata (select-keys (:metadata resource) [:name :namespace])
        read-op (openapi/api-name "read" resource)
        {err :error old-resource :result :as resp} (op ktx {:method read-op 
                                                      :params metadata})]
    (if (= 404 (:code err))
      (print-error (op ktx {:method (openapi/api-name "create" resource)
                            :params (cond-> {:body resource}
                                      (:namespace metadata)
                                      (assoc :namespace (:namespace metadata)))}))
      (let [diff (matcho/match*  old-resource resource)]
        (if (not (empty? diff))
          (print-error (op ktx
                           {:method (openapi/api-name "replace" resource)
                            :params (assoc metadata :body resource)}))
          (print-error resp))))))

(comment








  )
