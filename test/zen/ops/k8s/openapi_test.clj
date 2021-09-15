(ns zen.ops.k8s.openapi-test
  (:require [zen.ops.k8s.openapi :as sut]
            [zen.core :as zen]
            [zen.ops.k8s.core :as k8s]
            [matcho.core :as matcho]
            [clojure.test :as t]))



(t/deftest test-swagger-to-zen

  (def ztx (zen/new-context {}))
  (k8s/init-context ztx {})

  (def errors (take 10 (zen/errors ztx)))
  (t/is (empty? errors))

  errors

  (comment
    (sut/list-ops ztx "v1.pod")
    (sut/list-schemas ztx))

  (matcho/match
    (zen/get-symbol ztx 'k8s.v1/Pod)
    {:zen/tags #{'zen/schema 'k8s/schema},
     :zen/name 'k8s.v1/Pod})

  (matcho/match
    (zen/get-symbol ztx 'k8s.v1.Pod/list)
    {:zen/tags #{'k8s/op},
     :zen/name 'k8s.v1.Pod/list})

  (t/is (seq (zen/get-tag ztx 'k8s/op)))
  (t/is (seq (zen/get-tag ztx 'k8s/schema)))

  (matcho/match
    (zen/validate ztx #{'k8s.v1/Pod} {:metadata {:name 1}})
    {:errors
     [{:message "Expected type of 'string, got 'long"
       :type    "string.type"
       :path    [:metadata :name]}]})

  (sut/list-ops ztx "custom")
  (sut/list-schemas ztx "meta")


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

  (t/is
   (zen/get-symbol
    ztx
    (sut/api-name "read" {:apiVersion "apps/v1",
                          :kind       "Deployment",
                          :metadata   {:name "nginx-deployment", :labels {:app "nginx"}},
                          :spec {}})))

  (t/is
   (zen/get-symbol
    ztx
    (sut/api-name
     "read"
     {:k8s/type 'k8s.apps.v1/Deployment
      :kind       "Deployment",
      :metadata   {:name "nginx-deployment", :labels {:app "nginx"}},
      :spec {}})))
  

  (zen/get-symbol ztx 'k8s.batch.v2alpha1.CronJob/patch)
  (zen/get-symbol ztx 'k8s/get-v1-api-resources)

  (sut/list-schemas ztx "service")

  


  )
