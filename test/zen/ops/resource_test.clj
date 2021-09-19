(ns zen.ops.resource-test
  (:require [zen.ops.resource :as sut]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [cheshire.core :as cheshire]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(defmethod sut/expand
  'zen.ops.resource-test/App
  [ztx res]
  [{:kind "Deployment"
    :ns (:ns res)
    :template {:containers {:spec {:image (:image res)
                                   :env (:env res)}}}}
   {:kind "Service"
    :ns (:ns res)
    :port (:port res)}])

(defmethod sut/expand
  'zen.ops.resource-test/Sys
  [ztx res]
  [{:kind "Ingress" :port 300
    :namespace (:ns res)}
   {:zo/type 'zen.ops.resource-test/App
    :image (str "myapp:" (:version res))
    :ns (:ns res)
    :port 3000}])

(defmethod sut/expand
  'zen.ops.resource-test/Comp
  [ztx res]
  [{:zo/type 'zen.ops.resource-test/Sys
    :ns (str (:name res) "-edge")
    :version (str (:name res) "-edge")}
   {:zo/type 'zen.ops.resource-test/Sys
    :ns (str (:name res) "-prod")
    :version (str (:name res) "-prod")}])

(defmethod sut/format
  'zen.ops.resource-test/env
  [ztx _ envs]
  (->> envs
       (mapv (fn [[k v]] {:name (if (keyword? k) (subs (str k) 1) (str k))
                         :value (str v)}))))


(t/deftest test-resource
  (def ztx (zen/new-context {}))
  (zen/read-ns ztx 'zen.ops.resource-test)

  (def app (zen/get-symbol ztx 'zen.ops.resource-test/app))
  app

  (t/is (empty? (zen/errors ztx)))

  (matcho/match
   (sut/do-expand ztx app)

   [{:kind "Deployment"
     :template {:containers {:spec {:image "myimage"}}}}
    {:kind "Service"
     :port 8080}])

  ;; testing recursive expand
  (def sys (zen/get-symbol ztx 'zen.ops.resource-test/sys))

  sys

  (matcho/match
   (sut/do-expand ztx sys)
   [{:kind "Ingress", :port 300}
    {:kind "Deployment", :template {:containers {:spec {:image "myapp:2108"}}}}
    {:kind "Service", :port 3000}]
   )

  (def cmp (zen/get-symbol ztx 'zen.ops.resource-test/comp))
  (matcho/match
   (sut/do-expand ztx cmp)

   [{:kind "Ingress", :port 300, :namespace "samurai-edge"}
    {:kind "Deployment", :ns "samurai-edge", :template {:containers {:spec {:image "myapp:samurai-edge"}}}}
    {:kind "Service", :ns "samurai-edge", :port 3000}

    {:kind "Ingress", :port 300, :namespace "samurai-prod"}
    {:kind "Deployment", :ns "samurai-prod", :template {:containers {:spec {:image "myapp:samurai-prod"}}}}
    {:kind "Service", :ns "samurai-prod", :port 3000}])


  (matcho/match
   (sut/do-format
    ztx {:some-key {:nested {:zo/fmt 'zen.ops.resource-test/env
                             :ENV "one"
                             :TWO "two"}}})

   {:some-key {:nested [{:name "ENV", :value "one"}
                        {:name "TWO", :value "two"}]}})


  (def appf (zen/get-symbol ztx 'zen.ops.resource-test/app-fmt))
  appf

  (matcho/match
   (sut/do-expand ztx appf)
   [{:kind "Deployment",
     :ns "default",
     :template
     {:containers
      {:spec
       {:image "myimage",
        :env [{:name "HOME", :value "/home"} {:name "DATA", :value "/data"}]}}}}
    {:kind "Service", :ns "default", :port 8080}])


  )

