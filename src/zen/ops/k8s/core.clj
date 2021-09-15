(ns zen.ops.k8s.core
  (:require
   [zen.core :as zen]
   [zen.ops.k8s.openapi :as openapi]
   [zen.ops.k8s.exec]
   [org.httpkit.client :as http]
   [matcho.core :as matcho]
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
                                   :headers (cond-> {}
                                              t (assoc "Authorization" (str "Bearer " t)))}))
        (update :body (fn [x]
                        (when x (cheshire/parse-string x keyword)))))))

(defn load-cluster-api [ztx conn]
  (let [openapi-resp (request ztx conn {:method :get :url "openapi/v2"})]
    (if (= 200 (:status openapi-resp))
      (do
        (openapi/load-openapi ztx (:body openapi-resp))
        ztx)
      (throw (Exception. (pr-str openapi-resp))))))


(defn op [ztx conn req]
  (let [res (request ztx conn (build-request ztx req))]
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

(defn do-apply [ktx conn resource]
  (if (sequential? resource)
    (mapv (fn [res] (do-apply ktx conn res)) resource)
    (let [metadata (select-keys (:metadata resource) [:name :namespace])
          read-op (openapi/api-name "read" resource)
          create-op (openapi/api-name "create" resource)
          replace-op (openapi/api-name "replace" resource)
          resource (dissoc resource :k8s/type)
          {err :error old-resource :result :as resp} (op ktx conn {:method read-op :params metadata})]
      (print-error
       (if (= 404 (:code err))
         (op ktx conn
             {:method create-op
              :params (cond-> {:body resource}
                        (:namespace metadata)
                        (assoc :namespace (:namespace metadata)))})
         (let [diff (matcho/match*  old-resource resource)]
           (if (not (empty? diff))
             (op ktx conn
                 {:method replace-op
                  :params (assoc metadata :body resource)})
             resp)))))))

(comment

  (def conn {:url "http://localhost:8080"})

  (def ztx (zen/new-context {}))
  (init-context ztx {})

  (list-ops ztx "pod list")

  (->>
   (op ztx conn {:method 'k8s.v1.Pod/list-all})
   (items))

  (list-schemas ztx "namespac")

  (do-apply ztx conn
            {:k8s/type 'k8s.v1/Namespace
             :metadata {:name "test"
                        :labels {:managedBy "zenops"}}})

  (->>
   (op ztx conn {:method 'k8s.v1.Namespace/list})
   (items))




  )
