(ns zen.ops.gcp.core
  (:require [org.httpkit.client :as http]
            [zen.ops.jwt.core :as jwt]
            [zen.core :as zen]
            [clojure.string :as str]
            [cheshire.core :as json]))

(defn now [] (int (/ (System/currentTimeMillis) 1000)))

(defn service-account [path]
  (json/parse-string (slurp path) keyword))

(defn get-token [service-account opts]
  (let [now-t (now)
        claims {"iss" (:client_email service-account)
                "scope" (->> (:scope opts)
                             (str/join " "))
                "aud" (:token_uri service-account)
                "exp" (+ now-t (or (:expiration opts) 30))
                "iat" now-t}
        jwt (jwt/sign (jwt/get-only-private-key (jwt/read-key (:private_key sa))) claims :RS256)
        token-resp @(http/post (:token_uri service-account)
                               {:form-params {:grant_type "urn:ietf:params:oauth:grant-type:jwt-bearer"
                                              :assertion jwt}})]
    (if (= 200 (:status token-resp))
      (-> token-resp
          :body
          (json/parse-string keyword)
          :access_token)
      (throw (Exception. (pr-str token-resp))))))

(defn json-get [url & [opts]]
  (-> @(http/get url opts)
      :body
      (json/parse-string keyword)))

(defn load-api [ztx ])

(defn list-apis []
  (->> (json-get "https://discovery.googleapis.com/discovery/v1/apis")
       :items
       (reduce (fn [acc x]
                 (let [nm (keyword (:name x))]
                   (cond->
                       (assoc-in acc [nm (keyword (:version x))] (dissoc x :icons))

                     (:preferred x)
                     (assoc-in [nm :preferred] (dissoc x :icons))))))))

(defn load-api [disc]
  (json-get (:discoveryRestUrl disc)))

(defn list-operations [api]
  (->> (:resources api)
       (mapcat (fn [[res {ms :methods}]]
                 (->> ms
                      (mapv (fn [[m v]]
                              [res m])))))))

(defn request [ztx ctx api op]
  (-> @(http/request
        {:url (str (:baseUrl api) (:path op))
         :method (keyword (str/lower-case (:httpMethod op)))
         :headers {"accept" "application/json"
                   "authorization" (str "Bearer " (:token ctx))}})
      :body
      (json/parse-string keyword)))
