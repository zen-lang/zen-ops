(ns zen.ops.k8s.resources-test
  (:require [zen.ops.k8s.resources :as sut]
            [zen.ops.k8s.openapi :as openapi]
            [zen.ops.resource :as res]
            [zen.core :as zen]
            [clojure.java.io :as io]
            [matcho.core :as matcho]
            [cheshire.core]
            [clojure.walk]
            [clojure.test :as t]))


(t/deftest test-k8s-resources
  (def ztx (zen/new-context {}))
  (openapi/load-default-api ztx)

  (zen/load-ns ztx {'ns 'mysys

                    'import #{'zo 'zen.ops.k8s}

                    'myapp
                    {:zen/tags #{'zo/res}
                     :zo/type 'zen.ops.k8s/web-app
                     :image "nginx:latest"
                     :ns "my"
                     :url "myapp.samurai.io"
                     :port 8080}})




  ;;(t/is (empty? (take 10 (zen/errors ztx))))

  (def app (zen/get-symbol ztx 'mysys/myapp))


  (comment
    (openapi/describe  ztx 'k8s.batch.api.k8s.io.v1/JobSpec)
    (openapi/describe ztx 'k8s.networking.api.k8s.io.v1/Ingress)
    (openapi/describe ztx 'k8s.v1/Pod)
    )

  (matcho/match
   (res/do-expand ztx app)
   [{:k8s/type 'k8s.networking.api.k8s.io.v1/Ingress
     :metadata {:name "myapp" :namespace "my"}
     :spec {:rules [{:host "myapp.samurai.io"
                     :http {:paths [{:backend {:service {:name "myapp" :port 80}}
                                     :path "/"}]}}]}}
    {:k8s/type 'k8s.v1/Service
     :metadata {:name "myapp" :namespace "my"}
     :spec {:selector {:app "myapp"}
            :ports [{:protocol "TCP"
                     :port 80
                     :targetPort 8080}]}}

    {:k8s/type 'k8s.apps.v1/Deployment
     :metadata {:name "myapp" :namespace "my"}
     :spec {:replicas 1
            :selector {:matchLabels {:app "myapp"}}
            :template {:metadata {:labels {:app "myapp"}}
                       :spec {:containers [{:name "main"
                                            :image "nginx:latest"
                                            :ports [{:containerPort 8080}]}]}}}}])

  (openapi/list-schemas ztx "ingres")

  (->>
   (res/do-expand ztx app)
   (mapcat (fn [res]
           (:errors (zen/validate ztx #{(:k8s/type res)}
                          (dissoc res :k8s/type))))))


  (zen/load-ns ztx {'ns 'mysys

                    'import #{'zo 'zen.ops.k8s}

                    'myapp
                    {:zen/tags #{'zo/res}
                     :zo/type 'zen.ops.k8s/prometheus
                     :ns "monitoring"
                     :storage "100Gi"}})

  (def app (zen/get-symbol ztx 'mysys/myapp))

  (matcho/match
   (res/do-expand ztx app)
   
    [{:k8s/type 'k8s.v1/Service
      :metadata {:name "prometheus"
                 :namespace "monitoring"}}
  
     {:k8s/type 'k8s.rbac.authorization.k8s.io.v1/ClusterRole,
      :metadata {:name "prometheus"
                 :namespace "monitoring"},
      :rules
      [{:apiGroups [ "" ],
         :resources [ "nodes" "services" "endpoints" "pods" ],
        :verbs [ "get" "list" "watch" ]}
       {:apiGroups [ "" ], :resources [ "configmaps" ], :verbs [ "get" ]}
       {:nonResourceURLs [ "/metrics" ], :verbs [ "get" ]}
       {:apiGroups [ "" ], :resources [ "nodes/metrics" ], :verbs [ "get" ]}]}
  
     {:k8s/type 'k8s.rbac.authorization.k8s.io.v1/ClusterRoleBinding,
      :metadata {:name "prometheus"
                 :namespace "monitoring"},
      :roleRef
      {:apiGroup "rbac.authorization.k8s.io",
       :kind "ClusterRole",
       :name "prometheus"},
      :subjects [{:kind "ServiceAccount", :name "prometheus"} ]}
  
     {:k8s/type 'k8s.v1/Service
      :metadata {:name "prometheus"
                 :namespace "monitoring"},
      :spec
      {:ports [ {:name "promui", :protocol "TCP", :port 9090, :targetPort 9090} ],
       :type "NodePort"}}
  
     {:k8s/type 'k8s.v1/PersistentVolumeClaim
      :metadata {:name "prometheus-data"
                 :namespace "monitoring"},
      :spec
      {:accessModes [ "ReadWriteOnce" ], :resources {:requests {:storage "100Gi"}}}}
  
     {:k8s/type 'k8s.apps.v1/Deployment
      :metadata {:name "prometheus"
                 :namespace "monitoring"},
      :spec
      {:replicas 1,
       :strategy {:type "Recreate"},
       :template
       {:spec
        {:serviceAccountName "prometheus",
         :securityContext {:runAsUser 1000, :fsGroup 1000},
         :containers
         [ {:name "prometheus",
           :command [ "/bin/prometheus" ],
           :args
           [ "--config.file=/config/prometheus-config.yaml"
            "--storage.tsdb.path=/data/prometheus"
            "--storage.tsdb.retention.time=14d"
            "--web.console.libraries=/usr/share/prometheus/console_libraries"
            "--web.console.templates=/usr/share/prometheus/consoles"
            "--web.enable-lifecycle"
            "--web.route-prefix=/" ],
           :image "prom/prometheus",
           :imagePullPolicy "Always",
           :resources {:requests {:memory "1Gi"}, :limits {:memory "1Gi"}},
           :ports [ {:containerPort 9090} ],
           :volumeMounts
           [ {:mountPath "/data/prometheus", :name "prometheus-data"}
            {:mountPath "/config", :name "prometheus-config"} ]} ],
         :volumes
         [ {:name "prometheus-config", :configMap {:name "prometheus-config"}}
          {:name "prometheus-data",
           :persistentVolumeClaim {:claimName "prometheus-data"}} ]}}}}

{:k8s/type 'k8s.v1/ConfigMap
    :metadata {:name "prometheus-config"
               :namespace "monitoring"}
    :data {:prometheus-config.yaml string?}}
     ])

  )


;; grab schema from https://prometheus.io/docs/prometheus/latest/configuration/configuration/
(def config
  {:op/type 'zen.ops.k8s/prometheus-config
   :global {:scrape_interval "15s", :evaluation_interval "15s"},
   :scrape_configs [{:scrape_interval "10s"
                     :bearer_token_file "/var/run/secrets/kubernetes.io/serviceaccount/token"
                     :tls_config {:ca_file "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"}
                     :kubernetes_sd_configs [{:role "node"}]
                     :job_name "kubernetes-nodes-cadvisor"
                     :scrape_timeout "10s"
                     :relabel_configs [{:action "labelmap" :regex "__meta_kubernetes_node_label_(.+)"}]
                     :metric_relabel_configs [{:action "replace"
                                               :source_labels ["id"]
                                               :regex "^/machine\\.slice/machine-rkt\\\\x2d([^\\\\]+)\\\\.+/([^/]+)\\.service$"
                                               :target_label "rkt_container_name"
                                               :replacement "${2}-${1}"}
                                              {:action "replace"
                                               :source_labels ["id"]
                                               :regex "^/system\\.slice/(.+)\\.service$"
                                               :target_label "systemd_service_name"
                                               :replacement "${1}"}]
                     :metrics_path "/metrics/cadvisor"
                     :scheme "https"}]})


(def prometheus
  {:ops/type 'zen.ops.k8s/service
   :ops/vars {:name "prometheus"
              :data-dir "/data/prometheus"
              :config-dir "/config"
              :storage "100Gi"
              :image "prom/prometheus"}
   :name      '(var :name)
   :namespace '(var :name)
   ;; :replicas 1
   ;; :strategy {:type "Recreate"}
   :account {:ops/type 'zen.ops.k8s/account
             :clusterRole {:rules {:k8s.v1.ConfigMap #{:list}
                                   :k8s.v1.Node      #{:get :list}
                                   :k8s.v1.Metrics   #{:get}}}}
   :service {9090 {:k8s/ingress "monitoring.aidbox.app" :port 80}}
   :volumes {:data {:ops/type 'zen.ops.k8s/claim
                    :storage '(var :storage)
                    :mount {:path '(var :data-dir)}}
             :config {:ops/type 'zen.ops.k8s/configmap-volume
                      :mount {:path '(var :config-dir)}
                      :data config}}
   :image '(var :image)
   :command "/bin/prometheus"
   :memory "1Gi"
   :securityContext {:runAsUser 1000 :fsGroup 1000}
   :args {:ops/type 'zen.ops.k8s/long-opts
          :config.file '(path (var :config-dir) "prometheus-config.yaml")
          :storage.tsdb.path '(var :data-dir)
          :storage.tsdb.retention.time "14d"
          :web.console.libraries "/usr/share/prometheus/console_libraries"
          :web.console.templates "/usr/share/prometheus/consoles"
          :web.enable-lifecycle ""
          :web.route-prefix "/"}})
