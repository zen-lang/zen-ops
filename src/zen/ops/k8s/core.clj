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


(defn new-context [config]
  (let [ztx (zen.core/new-context {:kube/config config})
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

(defn update-resource [ktx resource]
  (let [metadata (select-keys (:metadata resource) [:name :namespace])
        read-op (openapi/api-name "get" resource)
        {err :error old-resource :result :as resp} (op ktx {:method read-op 
                                                      :params metadata})]
    (if (= 404 (:code err))
      (print-error (op ktx {:method (openapi/api-name "post" resource)
                            :params (cond-> {:body resource}
                                      (:namespace metadata)
                                      (assoc :namespace (:namespace metadata)))}))
      (let [diff (matcho/match*  old-resource resource)]
        (if (not (empty? diff))
          (print-error (op ktx
                           {:method (openapi/api-name "put" resource)
                            :params (assoc metadata :body resource)}))
          resp)))))

(comment
  (def ztx (new-context  {:kube/url   "http://localhost:8080"}))

  (list-ops ztx "statef")
  (list-ops ztx "deplo")
  (list-ops ztx "pod")
  (list-ops ztx "exec")

  (list-schemas ztx "statef")
  (list-schemas ztx "customr")


  (build-request ztx {:method 'io.k8s.api.apps.v1.Deployment/list
                      :params {:namespace "docs"}})

  (op-def ztx 'io.k8s.api.apps.v1.Deployment/list)


  (op-def ztx 'io.k8s.api.core.v1.PodExecOptions/connect)

  (->>
   (op ztx {:method 'io.k8s.api.apps.v1.Deployment/list
            :params {:namespace "docs"}})
   (items))

  (->>
   (op ztx {:method 'io.k8s.api.core.v1.Pod/list
            :params {:namespace "docs"}})
   (items))

  (->>
   (op ztx {:method 'io.k8s.api.core.v1.Pod/list
            :params {:namespace "cluster-production"}})
   (items [[:status :phase]]))

  (list-ops ztx "names")
  (list-ops ztx "cert lis")

  (->>
   (op ztx {:method 'io.k8s.api.core.v1.Namespace/list
            :params {:namespace "cluster-production"}})
   (items [[:status :phase]]))


  (op-def ztx 'io.k8s.api.core.v1.Namespace/get)

  (op ztx {:method 'io.k8s.api.core.v1.Namespace/get
           :params {:name "aidbox4yourcloud"}})


  (doseq [o (list-ops ztx "cert lis")]
    (let [d (op-def ztx o)]
      (when (get-in d [:params :namespace])
        (println o)
        (->>
         (op ztx {:method o :params {:namespace "docs"}})
         (items)
         (println)))))


  (list-ops ztx "exec")
  (list-ops ztx "file")
  (list-ops ztx "copy")
  (list-ops ztx)

  (->>
   (op ztx {:method 'io.ci3.v1.Build/list})
   :result
   :items
   (count))

  (future
    (doseq [b (->>
               (op ztx {:method 'io.ci3.v1.Build/list})
               :result
               :items
               ;;(count)
               #_(filter #(contains? #{"success" "failed"} (:status %))))]

      (println (op ztx {:method 'io.ci3.v1.Build/delete
                        :params {:name (get-in b [:metadata :name]) :namespace "default"}}))))


  (build-request ztx
                 {:method 'io.ci3.v1.Build/delete
                  :params {:name "alkona-1552647578751" :namespace "default"}})



  (op-def ztx 'io.k8s.api.core.v1.Pod/list)


  (list-ops ztx "exec")
  (op-def ztx 'io.k8s.api.core.v1.PodExecOptions/connect)

  (doseq [pod (-> (op ztx {:method 'io.k8s.api.core.v1.Pod/list
                           :params {:limit         10
                                    :fieldSelector "status.phase=Running"
                                    :namespace     "default"}})
                  (get-in [:result :items]))]
    (-> (exec ztx
              {:name      (get-in pod [:metadata :name])
               :namespace (get-in pod [:metadata :namespace])
               :params    {:command   ["ps" "aux"]
                           :container (get-in pod [:spec :containers 0 :name])
                           :stdout    "true"
                           :stderr    "true"}})
        (deref)
        :result
        (print)))









  )
