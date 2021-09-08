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
        jwt (jwt/sign (jwt/get-only-private-key (jwt/read-key (:private_key service-account))) claims :RS256)
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


(defn load-api-discovery [ztx]
  (->> (json-get "https://discovery.googleapis.com/discovery/v1/apis")
       :items
       (reduce (fn [acc x]
                 (let [nm      (:name x)
                       version (:version x)]
                   (zen/load-ns ztx
                                {'ns (symbol (str "gcp." nm "." version))
                                 'discovery 
                                 (-> (dissoc x :icons)
                                     (assoc :zen/tags #{'gcp/api-desc}))}))))))
(defn list-apis [ztx]
  (->> (zen/get-tag ztx 'gcp/api-desc)
       (sort)
       (mapv (fn [s]
               (or (zen/get-symbol ztx (symbol (namespace s) "def"))
                   (zen/get-symbol ztx (symbol (namespace s) "discovery")))))))

(defn url-template [url]
  (->> (str/split url #"/")
       (filterv #(not (str/blank? %)))
       (mapv (fn [x]
               (if (and (str/starts-with? x "{")
                        (str/ends-with? x "}"))
                 (keyword (subs x 1 (dec (count x))))
                 x)))))

(defmulti sch2zen (fn [x] (keyword (:type x))))

(defn ref2sym [ref]
  (-> ref
      (str/split #"/")
      last
      symbol))

(defn build-enum [vals descs]
  (->> vals
       (map-indexed (fn [i v]
                      {:value v :zen/desc (get descs i)}))
       (into [])))

(defn *sch2zen [{desc :description ref :$ref :as sch loc :location enum :enum enumdesc :enumDescriptions}]
  (-> (dissoc sch :description :$ref :location :enum :enumDescriptions)
      (cond-> desc (assoc :zen/desc desc)
              ref  (assoc :confirms #{(ref2sym ref)})
              loc (assoc :http/location (keyword loc))
              enum (assoc :enum (build-enum enum enumdesc)))
      (sch2zen)))

(defmethod sch2zen :default
  [sch]
  sch)

(defmethod sch2zen :string
  [sch]
  (let [f (:format sch)]
    (case f
      "google-datetime"
      (assoc (dissoc sch :format) :type 'zen/datetime)
      (assoc sch :type 'zen/string))))

(defmethod sch2zen :integer
  [sch]
  (assoc sch :type 'zen/integer))

(defmethod sch2zen :number
  [sch]
  (assoc sch :type 'zen/number))

(defmethod sch2zen :object
  [{req :required props :properties aprops :additionalProperties :as sch}]

  (let [reqs (into  #{} (mapv keyword req))]
    (cond->
        (merge
          (dissoc sch :properties :additionalProperties)
          {:type 'zen/map})
      props
      (assoc :keys (reduce (fn [acc [k v]]
                        (assoc acc k (*sch2zen v)))
                      {} props))


      aprops
      (assoc :values (*sch2zen aprops))

      (seq reqs)
      (assoc :required reqs))))

(defmethod sch2zen :array
  [{its :items :as sch}]
  (merge (dissoc sch :items)
         {:type  'zen/vector
          :every (*sch2zen its)}))

(defn to-zen-op [base-url {prms :parameters req :request :as op-def}]
  (let [base-url (str/replace base-url #"/$" "")
        params {:type 'zen/map
                :keys {}}
        params (->> prms
                    (reduce (fn [acc [k v]]
                              (cond-> 
                                  (assoc-in acc [:keys k] (*sch2zen v))
                                (:required v) (update :require (fn [x] (conj (or x #{}) k)))))
                            params))
        params (if req
                 (-> (assoc-in params [:keys :body] (*sch2zen req))
                     (update :require (fn [x] (conj (or x #{}) :body))))
                 params)]
    {:zen/tags #{'gcp/op}
     :zen/desc (:description op-def)
     :gcp/scopes (into #{} (:scopes op-def))
     :http/url  (into [base-url] (url-template (:flatPath op-def)))
     :http/method (keyword (str/lower-case (:httpMethod op-def)))
     :params params
     :result (*sch2zen (:response op-def))}))

(defn load-api-definition [ztx nm api-def]
  (let [bu (:baseUrl api-def)
        ns {'ns nm
            'def (dissoc api-def :resources :schemas)}
        ns  (->> (:schemas api-def)
                 (reduce (fn [acc [k v]]
                           (assoc acc (symbol k) (assoc (*sch2zen v) :zen/tags #{'gcp/schema 'zen/schema})))
                         ns))
        ns (->> (get-in api-def [:resources])
                (reduce (fn [acc [res {mths :methods}]]
                          (->> mths
                               (reduce (fn [acc [m op-def]]
                                         (assoc acc (symbol (str (name res) "-" (name m)))
                                                (assoc (to-zen-op bu op-def) :zen/tags #{'gcp/op})))
                                       acc)))
                        ns))]
    (zen/load-ns ztx ns)
    :ok))

(defn load-api [ztx nm]
  (if-let [{disc-url :discoveryRestUrl :as disc} (zen/get-symbol ztx (symbol (str nm) "discovery"))]
    (let [ns-name nm
          res (json-get disc-url)]
      (load-api-definition ztx nm res)
      res)
    {:error {:message (str "No api for " nm)}}))

(defn render-url [url-template params]
  (->> url-template
       (mapv (fn [x]
               (if (string? x)
                 x
                 (if-let [v (get params x)]
                   v
                   (throw (Exception. (pr-str :missed-param x params)))))))
       (str/join "/")))

(defn build-request [ztx op]
  (if-let [op-def (zen/get-symbol ztx (symbol (:method op)))]
    (let [{errs :errors} (zen/validate-schema ztx (:params op-def) (:params op))]
      (if-not (empty? errs)
        {:error {:errors errs}}
        (let [url (render-url (:http/url op-def) (:params op))
              body (when-let [b (get-in op [:params :body])] (cheshire.core/generate-string b))
              query (->> (:params op)
                         (reduce (fn [acc [k v]]
                                   (if (= :query (get-in op-def [:params :keys k :http/location]))
                                     (assoc acc k v)
                                     acc)
                                   ) {}))]
          (cond->
              {:url url
               :method (:http/method op-def)
               :query-params query
               :headers (cond-> {}
                          body (assoc "content-type" "application/json"))}
            body (assoc :body body)))))
    {:error {:message (str "No op definition " (:method op))}}))

(defn op [ztx token op]
  (let [{err :error :as req} (-> (build-request ztx op) (assoc-in [:headers "authorization"] (str "Bearer " token)))]
    (if err
      req
      (let [resp (-> @(http/request req)
                     (update :body (fn [x] (when x (json/parse-string x keyword)))))]
        (if (< (:status resp) 300)
          {:result (:body resp)}
          {:error (:body resp)})))))
