(ns zen.ops.gcp.core-test
  (:require [zen.ops.gcp.core :as sut]
            [zen.core :as zen]
            [matcho.core :as matcho]
            [cheshire.core :as cheshire]
            [clojure.test :as t]))

(def def-json (cheshire.core/parse-string (slurp "libs/zen-ops/test/zen/ops/gcp/gcp.cloudresourcemanager.v1.json") keyword))
(def k8s-def-json (cheshire.core/parse-string (slurp "libs/zen-ops/test/zen/ops/gcp/gcp.container.v1.json") keyword))

;; (def def-json (cheshire.core/parse-string (slurp "test/zen/ops/gcp/gcp.cloudresourcemanager.v1.json") keyword))

(keys def-json)

(t/deftest test-gcp-api-parsing
  (def ztx (zen/new-context {}))

  (sut/load-api-definition ztx 'gcp.cloudresourcemanager.v1 def-json)


  (zen/get-tag ztx 'gcp/op)

  (matcho/match
   (zen/get-symbol ztx 'gcp.cloudresourcemanager.v1/projects-create)
   {:zen/name 'gcp.cloudresourcemanager.v1/projects-create
    :zen/tags #{'gcp/op}
    ;; :id "cloudresourcemanager.projects.create"
    :gcp/scopes #{"https://www.googleapis.com/auth/cloud-platform"}
    :http/url  [string? "v1" "projects"]
    :http/method :post
    :zen/desc string?
    :params {:type 'zen/map
             :keys {:body {:confirms #{'gcp.cloudresourcemanager.v1/Project}}}}

    :result {:confirms #{'gcp.cloudresourcemanager.v1/Operation}}})

  (matcho/match
   (zen/get-symbol ztx 'gcp.cloudresourcemanager.v1/projects-list)
   {:zen/tags #{'gcp/op}
    :zen/desc "Lists Projects that the caller has the `resourcemanager.projects.get` permission on and satisfy the specified filter. This method returns Projects in an unspecified order. This method is eventually consistent with project mutations; this means that a newly created project may not appear in the results or recent updates to an existing project may not be reflected in the results. To retrieve the latest state of a project, use the GetProject method. NOTE: If the request filter contains a `parent.type` and `parent.id` and the caller has the `resourcemanager.projects.list` permission on the parent, the results will be drawn from an alternate index which provides more consistent results. In future versions of this API, this List method will be split into List and Search to properly capture the behavioral difference.",
    :zen/name 'gcp.cloudresourcemanager.v1/projects-list,
    :http/method :get
    :http/url [string? "v1" "projects"]
    :params {:type 'zen/map
             :keys {:pageToken {:type 'zen/string
                                :http/location :query
                                :zen/desc #"Optional"}
                    :pageSize {:http/location :query
                               :type 'zen/integer}
                    :filter {:type 'zen/string
                             :http/location :query}}}})

  (matcho/match
   (zen/get-symbol ztx 'gcp.cloudresourcemanager.v1/projects-get)
   {:zen/tags #{'gcp/op}
    :zen/name 'gcp.cloudresourcemanager.v1/projects-get
    :result {:confirms #{'gcp.cloudresourcemanager.v1/Project}} 
    :http/url [string? "v1" "projects" :projectId]
    :http/method :get
    :params {:type 'zen/map
             :require #{:projectId}
             :keys {:projectId {:type 'zen/string :http/location :path}}}})

  (matcho/match
   (zen/get-symbol ztx 'gcp.cloudresourcemanager.v1/Project)
   {:zen/tags #{'zen/schema 'gcp/schema}
    :zen/name 'gcp.cloudresourcemanager.v1/Project
    :type 'zen/map
    :id "Project"
    :keys {:labels {:type 'zen/map
                    :values {:type 'zen/string}}
           :createTime {:type 'zen/datetime}
           :parent {:confirms #{'gcp.cloudresourcemanager.v1/ResourceId}}
           :lifecycleState {:type 'zen/string
                            :enum [{:value "LIFECYCLE_STATE_UNSPECIFIED"}
                                   {:value  "ACTIVE"}
                                   {:value  "DELETE_REQUESTED"}
                                   {:value  "DELETE_IN_PROGRESS"
                                    :zen/desc "This lifecycle state is no longer used and not returned by the API."}]}
           :projectId {:type 'zen/string}
           :projectNumber {:format "int64"
                           :type 'zen/string}
           :name {:type 'zen/string}}})

  (matcho/match
   (sut/build-request ztx 
                      {:method 'gcp.cloudresourcemanager.v1/projects-get
                       :params {}})
   {:error
    {:errors
     [{:message ":projectId is required",
       :type "require",
       :path [:projectId],
       :schema [:require]}]}})

  (matcho/match
   (sut/build-request ztx 
                      {:method 'gcp.cloudresourcemanager.v1/projects-get
                       :params {:projectId "myproj"}})

   {:url "https://cloudresourcemanager.googleapis.com/v1/projects/myproj"
    :method :get})

  (matcho/match
   (sut/build-request ztx
                      {:method 'gcp.cloudresourcemanager.v1/projects-create
                       :params {:body {:projectId "myproj"}}})

   {:url "https://cloudresourcemanager.googleapis.com/v1/projects",
    :method :post,
    :headers {"content-type" "application/json"},
    :body "{\"projectId\":\"myproj\"}"})



  (zen/get-symbol ztx 'gcp.cloudresourcemanager.v1/projects-list)

  (matcho/match
   (sut/build-request ztx {:method 'gcp.cloudresourcemanager.v1/projects-list
                           :params {:filter "labels.system:aidbox"}})

   {:url "https://cloudresourcemanager.googleapis.com/v1/projects",
    :method :get,
    :query-params {:filter "labels.system:aidbox"},
    :headers {}})

  (sut/load-api-definition ztx 'gcp.container.v1 k8s-def-json)

  
  (:methods (:zones (:resources (:projects (:resources k8s-def-json)))))
  
  (zen/get-tag ztx 'gcp/schema)

  (:methods (:clusters (:resources (:locations (:resources (:projects (:resources k8s-def-json)))))))
  
  (zen/get-symbol ztx 'gcp.container.v1/Cluster)

  )
