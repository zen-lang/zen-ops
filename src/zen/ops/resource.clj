(ns zen.ops.resource)

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

;; recursive expand (update in-place or append new resource)
;; aggregation (add resources based on result of expansion)

(declare do-expand)

(defmulti expand (fn [ztx resource] (:zo/type resource)))


(defn do-expand [ztx resource]
  (->> (expand ztx resource)
       (reduce (fn [acc res]
                 (if (:zo/type res)
                   (into acc (do-expand ztx res))
                   (conj acc res)))
               [])))

