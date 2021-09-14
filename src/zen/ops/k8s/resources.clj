(ns zen.ops.k8s.resources
  (:require [zen.ops.resource :as res]))

(defmethod res/expand
  'zen.ops.k8s/web-app
  [ztx {ns :ns url :url port :port img :image :as res}]
  (let [nm (name (:zen/name res))]
    [{:k8s/type 'k8s.networking.api.k8s.io.v1/Ingress
      :metadata {:name nm :namespace ns}
      :spec {:rules [{:host url
                      :http {:paths [{:backend {:service {:name nm :port 80}}
                                      :path "/"}]}}]}}
     {:k8s/type 'k8s.v1/Service
      :metadata {:name nm :namespace ns}
      :spec {:selector {:app nm}
             :ports [{:protocol "TCP"
                      :port 80
                      :targetPort port}]}}

     {:k8s/type 'k8s.apps.v1/Deployment
      :metadata {:name nm :namespace ns}
      :spec {:replicas 1
             :selector {:matchLabels {:app nm}}

             :template {:metadata {:labels {:app nm}}
                        :spec {:containers [{:name "main"
                                             :image img
                                             :ports [{:containerPort port}]}]}}}}
     ])

  )
