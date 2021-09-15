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




  (t/is (empty? (take 10 (zen/errors ztx))))


  (def app (zen/get-symbol ztx 'mysys/myapp))

  (comment
    (openapi/describe  ztx 'k8s.batch.api.k8s.io.v1/JobSpec)
    (openapi/describe ztx 'k8s.networking.api.k8s.io.v1/Ingress)
    (openapi/describe ztx 'k8s.v1/Pod))

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

  (openapi/list-schemas ztx "cluster")

  (->>
   (res/do-expand ztx app)
   (mapcat (fn [res]
             (:errors (zen/validate ztx #{(:k8s/type res)}
                                    (dissoc res :k8s/type)))))))
