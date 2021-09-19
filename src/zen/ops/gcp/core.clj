(ns zen.ops.gcp.core
  (:require [org.httpkit.client :as http]
            [zen.ops.jwt.core :as jwt]
            [zen.core :as zen]
            [clojure.walk]
            [clojure.string :as str]
            [cheshire.core :as json]))

(defn now [] (int (/ (System/currentTimeMillis) 1000)))

(defn service-account [path]
  (json/parse-string (slurp path) keyword))

(defn get-token [service-account & [opts]]
  (let [now-t (now)
        claims {"iss" (:client_email service-account)
                "scope" (->> (or (:scope opts)
                                 ["https://www.googleapis.com/auth/cloud-platform" "https://www.googleapis.com/auth/cloud-platform.read-only"])
                             (str/join " "))
                "aud" (:token_uri service-account)
                "exp" (+ now-t (or (:exp opts) 30))
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

(defn token [path]
  (get-token (service-account "gcp-creds.json")))

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


(defn parse-url-name [x]
  (keyword (str/replace x #"(\{|\+|\})" "")))

(defn url-template [url]
  (->> (str/split url #"/")
       (filterv #(not (str/blank? %)))
       (mapv (fn [x]
               (if (and (str/starts-with? x "{"))
                 (if (str/includes? x ":")
                   (let [[p op] (str/split x #":" 2)]
                     [(parse-url-name p) op])
                   (parse-url-name x))
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
      (cond-> (assoc sch :type 'zen/string)
          (:pattern sch) (-> (dissoc :pattern) (assoc :regex (:pattern sch)))))))

(defmethod sch2zen :integer
  [sch]
  (assoc sch :type 'zen/integer))

(defmethod sch2zen :boolean
  [sch]
  (assoc sch :type 'zen/boolean))

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

(defn to-zen-op [{base-url :base-url pth :path} {prms :parameters req :request :as op-def}]
  (when (not (= (:flatPath op-def)
                (:path op-def)))
    #_(println (:id op-def) pth
             "\n>"
             (:flatPath op-def)
             "\n>"
             (:path op-def)))
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
     :http/url  (into [base-url] (url-template (:path op-def)))
     :http/xurl  (into [base-url] (url-template (:flatPath op-def)))
     :http/method (keyword (str/lower-case (:httpMethod op-def)))
     ;; :source op-def
     :params params
     :result (*sch2zen (:response op-def))}))

(defn recursive-apis [acc {pth :path bu :base-url :as ctx} api-def]
  (let [acc (->> (:methods api-def)
                 (reduce
                  (fn [acc [m op-def]]
                    (assoc acc (symbol (str (str/join "-" (conj pth (name m)))))
                           (assoc (to-zen-op ctx op-def) :zen/tags #{'gcp/op})))
                  acc))]
    (->> (:resources api-def)
         (reduce (fn [acc [res api]]
                   (let [ctx (update ctx :path conj (name res))]
                     (recursive-apis acc ctx api)))
                 acc))))

(defn load-api-definition [ztx nm api-def]
  (let [bu (:baseUrl api-def)
        ns {'ns nm
            'def (dissoc api-def :resources :schemas)}
        ns  (->> (:schemas api-def)
                 (reduce (fn [acc [k v]]
                           (assoc acc (symbol k) (assoc (*sch2zen v) :zen/tags #{'gcp/schema 'zen/schema})))
                         ns))
        ns (recursive-apis ns {:path [] :base-url bu} api-def)]
    (zen/load-ns ztx ns)
    :ok))

(defn get-api [ztx nm]
  (if-let [{disc-url :discoveryRestUrl :as disc} (zen/get-symbol ztx (symbol (str nm) "discovery"))]
    (json-get disc-url)
    {:error {:message (str "No api for " nm)}}))

(defn load-api [ztx nm]
  (let [res (get-api ztx nm)]
    (if (:error res)
      res
      (load-api-definition ztx nm res))))

(defn get-url-param [params x]
  (if-let [v (get params x)] v (throw (Exception. (pr-str :missed-param x params)))))

(defn render-url [url-template params]
  (->> url-template
       (mapv (fn [x]
               (cond
                 (string? x) x
                 (keyword? x)  (get-url-param params x)
                 (vector? x) (str (get-url-param params (first x)) ":" (second x)))))
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
                     (update :body (fn [x] (when x (try (json/parse-string x keyword)
                                                       (catch Exception e
                                                         x))))))]
        (if (nil? (:status resp))
          {:error resp}
          (if (< (:status resp) 300)
            {:result (:body resp)}
            {:error (let [b (:body resp)]
                      (or (:error b) b))}))))))


(defn build-filter [q]
  (when q (re-pattern (str ".*" (str/join ".*" (mapv str/lower-case (str/split q #"\s+"))) ".*"))))

(defn ilike-filter [q xs]
  (let [q (build-filter q)]
    (->> (cond->> xs
           q (filterv (fn [x]
                        (not (nil? (re-matches q (str/lower-case (str x))))))))
         (sort))))

(defn list-ops [ztx & [q]]
  (->> (zen/get-tag ztx 'gcp/op)
       (ilike-filter q)))

(defn list-schemas [ztx & [q]]
  (->> (zen/get-tag ztx 'gcp/schema)
       (ilike-filter q)))

(declare effective-schema)

(defn effective-schema* [ztx sch]
  (clojure.walk/postwalk
   (fn [x]
     (if-let [cfrm  (and (map? x) (first (:confirms x)))]
       (merge x (effective-schema ztx cfrm))
       x))
   sch))

(defn effective-schema [ztx sym]
  (effective-schema* ztx (zen/get-symbol ztx sym)))

(defn gen-sample [sch]
  (cond
    (= 'zen/map (:type sch))
    (->> (:keys sch)
         (reduce (fn [acc [k v]]
                   (assoc acc k (gen-sample v))
                   ) {}))

    (= 'zen/vector (:type sch))
    [(gen-sample (:every sch))]

    :else
    (or (:type sch) sch)))

(defn describe [ztx sym]
  (gen-sample (effective-schema ztx sym)))

(defn op-def [ztx op-name]
  (let [op (zen/get-symbol ztx op-name)]
    op))

(defn op-desc [ztx op-name]
  (let [op (zen/get-symbol ztx op-name)]
    {:method op-name
     :params (gen-sample (effective-schema* ztx (:params op)))}))
