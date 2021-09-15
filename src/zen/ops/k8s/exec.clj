(ns zen.ops.k8s.exec
  (:require
   [zen.core]
   [zen.ops.k8s.openapi :as openapi]
   [clojure.walk]
   [org.httpkit.client :as http]
   [clojure.java.io :as io]
   [json-schema.core :as json-schema]
   [cheshire.core :as cheshire]
   [clojure.string :as str])
  (:import [java.util Base64]
           [java.net URLEncoder]
           (java.nio ByteBuffer)
           [org.eclipse.jetty.websocket.api WebSocketListener
            Session
            WriteCallback RemoteEndpoint]
           [javax.net.ssl SSLEngine]
           [org.httpkit.client ClientSslEngineFactory]
           [org.eclipse.jetty.util.ssl SslContextFactory]
           [org.eclipse.jetty.websocket.client
            WebSocketClient
            ClientUpgradeRequest]))

(defonce wsc (atom nil))
(defn get-client []
  (or @wsc (reset! wsc
                   (let [sslf (SslContextFactory.)
                         _ (.setTrustAll sslf true)
                         ws (WebSocketClient. sslf)]
                     (.start ws)
                     ws))))


(comment
 (.stop @wsc)
 (reset! wsc nil)
 )


(defn ws-listener
  [p opts]
  (let [out (java.io.ByteArrayOutputStream.)]
    (reify WebSocketListener
      (onWebSocketConnect [this session]
        (println "Session" session))
      (onWebSocketText [this message]
        (throw (UnsupportedOperationException. "Text not supported")))
      (onWebSocketBinary [this bytes offset len]
        (println :binary offset len)
        (.write out (.array (ByteBuffer/wrap bytes offset len))))
      (onWebSocketError [this cause]
        (deliver p {:error {:message (.getMessage cause) :opts opts}}))
      (onWebSocketClose [this status-code reason]
        (deliver p {:result (String. (.toByteArray out))})))))

(defn url-encode [x]
  (URLEncoder/encode x "UTF-8"))

(defn query-string [params]
  (->> params
       (reduce
         (fn [acc [k v]]
           (if (sequential? v)
             (into acc (->> v (mapv #(str (name k)"="(url-encode %)))))
             (conj acc (str (name k)"="(url-encode v)))
             ))
         [])
       (str/join "&")))

(defn exec [ztx conn opts]
  (let [req (ClientUpgradeRequest.)
        url (:url conn)
        _ (when-let [t (:token conn)]
            (.setHeader req "Authorization" (str "Bearer " t)))
        query (query-string (:params opts))
        uri (-> (str url "/" (zen.ops.k8s.openapi/render-url ["api" "v1" "namespaces" :namespace "pods" :name "exec"] opts))
                (str/replace  #"^http" "ws")
                (str "?" query))]
    (let [p (promise)]
      (.connect (get-client) (ws-listener p {:uri uri})
                (java.net.URI. uri)
                req)
      p)))
