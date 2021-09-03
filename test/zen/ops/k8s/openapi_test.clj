(ns zen.ops.k8s.openapi-test
  (:require [zen.ops.k8s.openapi :as sut]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [cheshire.core :as cheshire]
            [clojure.test :as t]))



(def swagger (cheshire.core/parse-string (slurp "test/zen/ops/k8s/swagger.json") keyword))

(t/deftest test-swagger-to-zen

  (def ztx (zen/new-context {}))

  ;; (def nss (sut/load-namespaces ztx swagger))
  ;; (->> (keys nss) (sort))

  ;; (get nss 'io.k8s.api.core.v1)

  (sut/load-openapi ztx swagger)

  (matcho/match
    (zen/get-symbol ztx 'io.k8s.api.core.v1/Pod)
    {:zen/tags #{'zen/schema 'k8s/schema},
     :zen/name 'io.k8s.api.core.v1/Pod})

  (matcho/match
    (zen/get-symbol ztx 'io.k8s.api.core.v1.Pod/list)
    {:zen/tags #{'k8s/op},
     :zen/name 'io.k8s.api.core.v1.Pod/list})

  ;; apps.v1.StatefulSet/post
  ;; 'io.k8s.api.apps.v1/StatefulSetStatus
  ;;  io.k8s.api.apps.v1.StatefulSet/deletecollection

  ;; batch.v1.Job/watch
  ;; 'io.k8s.api.batch.v1/Job

  (t/is (seq (zen/get-tag ztx 'k8s/op)))
  (t/is (seq (zen/get-tag ztx 'k8s/schema)))

  (matcho/match
    (zen/validate ztx #{'io.k8s.api.core.v1/Pod} {:metadata {:name 1}})
    {:errors
     [{:message "Expected type of 'string, got 'long"
       :type    "string.type"
       :path    [:metadata :name]
       :schema
       ['io.k8s.api.core.v1/Pod :metadata :confirms 'io.k8s.apimachinery.pkg.apis.meta.v1/ObjectMeta :name]}]})



  (sut/list-ops ztx "custom")
  (sut/list-schemas ztx "custom")

  (zen/get-symbol ztx
                  'io.k8s.apimachinery.pkg.apis.meta.v1/LabelSelector)

  (matcho/match
    (sut/validate
      ztx
      {:apiVersion "apps/v1",
       :kind       "Deployment",
       :metadata   {:name "nginx-deployment", :labels {:app "nginx"}},
       :spec
       {:replicas 3,
        :selector {:matchLabels {:app "nginx"}},
        :template
        {:metadata {:labels {:app "nginx"}},
         :spec
         {:containers
          [{:name  "nginx",
            :image "nginx:1.14.2",
            :ports [{:containerPort 80}]}]}}}})
    {:errors empty?})

  (matcho/match
    (sut/validate
      ztx
      {:apiVersion "apps/v1",
       :kind       "Deployment",
       :metadata   {:name "nginx-deployment", :labels {:app "nginx"}},
       :spec
       {:replicas 3,
        :selector {:matchLabels {:app "nginx"}},
        :template
        {:metadata {:labels {:app "nginx"}},
         :spec
         {:containers
          [{:name  "nginx",
            :image "nginx:1.14.2",
            :ports [{:containerPort "80"}]}]}}}})
    {:errors
     [{:message "Expected type of 'integer, got 'string",
       :type    "primitive-type",
       :path    [:spec :template :spec :containers 0 :ports 0 :containerPort],}]})

  (sut/validate
    ztx
    {:apiVersion "apiextensions.k8s.io/v1",
     :kind       "CustomResourceDefinition",
     :metadata   {:name "crontabs.stable.example.com"},
     :spec       {:group    "stable.example.com",
                  :versions [{:name    "v1",
                              :served  true,
                              :storage true,
                              :schema
                              {:openAPIV3Schema
                               {:type       "object",
                                :properties {:spec
                                             {:type       "object",
                                              :properties {:cronSpec {:type "string"},
                                                           :image    {:type "string"},
                                                           :replicas {:type "integer"}}}}}}}],
                  :scope    "Namespaced",
                  :names    {:plural     "crontabs",
                             :singular   "crontab",
                             :kind       "CronTab",
                             :shortNames ["ct"]}}})



  )
