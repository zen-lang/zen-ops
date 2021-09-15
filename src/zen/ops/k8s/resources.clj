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
                                             :ports [{:containerPort port}]}]}}}}]))

(defmethod res/expand
  'zen.ops.k8s/prometheus
  [ztx {ns :ns}]
  [{:k8s/type 'k8s.v1/Service
    :metadata {:name "prometheus"
               :namespace ns}}

   {:k8s/type 'k8s.rbac.authorization.k8s.io.v1/ClusterRole,
    :metadata {:name "prometheus" :namespace ns},
    :rules
    [{:apiGroups [""],
      :resources ["nodes" "services" "endpoints" "pods"],
      :verbs ["get" "list" "watch"]}
     {:apiGroups [""], :resources ["configmaps"], :verbs ["get"]}
     {:nonResourceURLs ["/metrics"], :verbs ["get"]}
     {:apiGroups [""], :resources ["nodes/metrics"], :verbs ["get"]}]}

   {:k8s/type 'k8s.rbac.authorization.k8s.io.v1/ClusterRoleBinding,
    :metadata {:name "prometheus"
               :namespace ns},
    :roleRef
    {:apiGroup "rbac.authorization.k8s.io",
     :kind "ClusterRole",
     :name "prometheus"},
    :subjects [{:kind "ServiceAccount", :name "prometheus"}]}

   {:k8s/type 'k8s.v1/Service
    :metadata {:name "prometheus"
               :namespace ns},
    :spec
    {:ports [{:name "promui", :protocol "TCP", :port 9090, :targetPort 9090}],
     :type "NodePort"}}

   {:k8s/type 'k8s.v1/PersistentVolumeClaim
    :metadata {:name "prometheus-data"
               :namespace ns},
    :spec
    {:accessModes ["ReadWriteOnce"], :resources {:requests {:storage "100Gi"}}}}

   {:k8s/type 'k8s.apps.v1/Deployment
    :metadata {:name "prometheus"
               :namespace ns},
    :spec
    {:replicas 1,
     :strategy {:type "Recreate"},
     :template
     {:spec
      {:serviceAccountName "prometheus",
       :securityContext {:runAsUser 1000, :fsGroup 1000},
       :containers
       [{:name "prometheus",
         :command ["/bin/prometheus"],
         :args
         ["--config.file=/config/prometheus-config.yaml"
          "--storage.tsdb.path=/data/prometheus"
          "--storage.tsdb.retention.time=14d"
          "--web.console.libraries=/usr/share/prometheus/console_libraries"
          "--web.console.templates=/usr/share/prometheus/consoles"
          "--web.enable-lifecycle"
          "--web.route-prefix=/"],
         :image "prom/prometheus",
         :imagePullPolicy "Always",
         :resources {:requests {:memoryory "1Gi"}, :limits {:memory "1Gi"}},
         :ports [{:containerPort 9090}],
         :volumeMounts
         [{:mountPath "/data/prometheus", :name "prometheus-data"}
          {:mountPath "/config", :name "prometheus-config"}]}],
       :volumes
       [{:name "prometheus-config", :configMap {:name "prometheus-config"}}
        {:name "prometheus-data",
         :persistentVolumeClaim {:claimName "prometheus-datkka"}}]}}}}])

(comment
  (map clj-yaml.core/parse-string (clojure.string/split (slurp "configuration.yaml") #"---")))

