(ns klog.core
  (:require [cheshire.core]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.stacktrace :as stacktrace]
            [org.httpkit.client :as http]
            [clojure.string :as str])
  (:import java.time.Clock
           (java.time LocalDate format.DateTimeFormatter)
           java.util.zip.GZIPOutputStream
           java.io.OutputStreamWriter
           java.io.FileOutputStream
           java.util.TimeZone
           java.text.SimpleDateFormat
           java.io.Writer
           java.io.BufferedWriter))


;; (set! *warn-on-reflection* true)

(def fmt (let [tz (java.util.TimeZone/getTimeZone "UTC")
               df (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")]
           (.setTimeZone df tz)
           df))

(defn format-date [^java.util.Date x]
  (str (.format ^java.text.SimpleDateFormat fmt  x)))

(defonce ^:dynamic *enable* (if (System/getenv "KLOG_DISABLE") false true))

(defn enable-log [] (alter-var-root #'*enable* (constantly true)))

(defn disable-log [] (alter-var-root #'*enable* (constantly false)))

(defonce ^:dynamic *warn-unknown-event* false) ;; TODO: add option to enable

(defonce publisher (agent nil))

(comment

  (do
    (restart-agent publisher nil)
    :ups)

  )

;; TODO: it's better to have one ThreadLocal map instead of -op, -ctx, -tn...
;; But interface could be saved (get-ctx, set-ctx...)
;; nicola: Looks like it's better to pass ctx into logs - but we have to refactor all log calls

(def -op (ThreadLocal.))

(defn set-op [x] (.set ^ThreadLocal -op x))

(defn get-op [] (.get ^ThreadLocal -op))

(defn clear-op []
  (.set ^ThreadLocal -op nil))

(def -ctx (ThreadLocal.))

(defn set-ctx [v]
  (.set ^ThreadLocal -ctx v))

(defn get-ctx []
  (.get ^ThreadLocal -ctx))

(defn clear-ctx []
  (.set ^ThreadLocal -ctx nil))

(def -tn (ThreadLocal.))

(defn set-tn [v]
  (.set ^ThreadLocal -tn v))

(defn clear-tn []
  (.set ^ThreadLocal -tn nil))

(defn mk-log [ev arg]
  (let [i   (format-date (java.util.Date.))
        w   (.getName (Thread/currentThread))
        tn  (.get ^ThreadLocal  -tn)
        ctx (.get ^ThreadLocal -ctx)
        op  (.get ^ThreadLocal -op)
        log (cond-> (assoc arg :ts i :w w :ev ev)
              tn  (assoc :tn tn)
              ctx (assoc :ctx ctx)
              op  (assoc :op op))]
    log))

(defonce appenders (atom {}))

(defn clear-appenders []
  (reset! appenders {}))

(def level-priorities
  {:off 0
   :fatal 100
   :error 200
   :warn 300
   :info 400
   :debug 500
   :trace 600
   :all Integer/MAX_VALUE})


(defn filter-log-by-lvl [appender-lvl-num f]
  (fn [l]
    (let [lvl-num (get level-priorities (or (:lvl l) :info) 0)]
      (when (<= lvl-num appender-lvl-num)
        (f l)))))

(defn add-appender
  ([k f]
   (add-appender k :all f))
  ([k lvl f]
   (assert (contains? level-priorities lvl) (str lvl " level not supported. Pick one of " (str (keys level-priorities))))
   (swap! appenders assoc k (filter-log-by-lvl (get level-priorities lvl) f))))

(defn rm-appender [k]
  (swap! appenders dissoc k))

(defn append-log [^java.io.Writer w l]
  (try
    (cheshire.core/generate-stream l w)
    (.write w "\n")
    (.flush w)
    (catch Exception e
      (println "ERROR while logging" e))))

(defn stdout-appender
  ([]
   (stdout-appender :all))
  ([lvl]
   (add-appender :stdout lvl (fn [l] (println (cheshire.core/generate-string l))))))



(defn green [x]
  (str "\033[0;32m" x "\033[0m"))

(defn gray [x]
  (str "\033[0;37m" x "\033[0m"))

(defn yellow [x]
  (str "\033[0;33m" x "\033[0m"))

(defn white [x]
  (str "\033[0;97m" x "\033[0m"))

(defn cyan [x]
  (str "\033[0;36m" x "\033[0m"))

(defn red [x]
  (str "\033[0;31m" x "\033[0m"))

(defn format-line [l]
  (try 
    (let [s (cond-> []
              (:tn l) (conj (:tn l))
              (:ts l) (conj (subs (:ts l) 11 19))
              (:lvl l) (conj (:lvl l))
              (:w l)  (conj (gray
                             (let [w (:w l)
                                   c (count w)]
                               (if (> c 5)
                                 (subs w (- c 5) c)
                                 w))))
              (:d l)  (conj (str "[" (:d l) "ms]"))
              (:err l) (conj (red (:err l))))

          s (cond
              (= :w/req (:ev l))
              (conj s (yellow (str/upper-case (name (or (:w_m l) "get"))))
                    (if-let [qs (:w_qs l)]
                      (str (white (:w_url l)) "?" qs)
                      (white (:w_url l))))

              (= :w/resp (:ev l))
              (conj s (yellow (:w_st l)))

              (= :auth/authorized-access-policy (:ev l))
              (conj s
                    (white "policy")
                    (green (when-let [id (:access-policy-id l)]
                             (name id))))

              (= :resource/create (:ev l))
              (conj s
                    (white "create")
                    (green (str (:rtp l) "/" (:rid l))))

              (= :resource/update (:ev l))
              (conj s
                    (white "update")
                    (green (str (:rtp l) "/" (:rid l))))

              (= :resource/delete (:ev l))
              (conj s
                    (white "delete")
                    (green (str (:rtp l) "/" (:rid l))))

              (or (= :db/q (:ev l))
                  (= :db/ex (:ev l)))
              (conj s (cyan (or (:sql l) ""))
                    (:db_prm l))

              (or (= :w/ex (:ev l)))
              (conj s
                    (red (:msg l))
                    (red (:etr l)))

              :else (conj s (yellow (str (:ev l))) (dissoc l :tn :ts :ev :w :lvl :error)))]
      (str/join  " " s))
    (catch Exception e
      (println "UPS EX IN LOGS: " e))))

(defn pretty-appender [l]
  (println (format-line l)))


(defn stdout-pretty-appender
  ([] (stdout-pretty-appender :all))
  ([lvl]
   (let [ctx (atom nil)]
     (add-appender :pretty-stdout lvl pretty-appender))))

(defn gz-writer [pth]
  (let [w  (java.io.FileWriter. ^String pth true)]
    (java.io.BufferedWriter. w)))

;; FIXME: it doesn't allow to specify log lvl
(defn file-appender [path & [max-lines]]
  (let [w         (atom (gz-writer path))
        max-lines (or max-lines 10000)
        i         (atom 0)]
    (add-appender :file :all (fn [l]
                               (let [^java.io.Writer wr @w]
                                 (if (<= max-lines @i)
                                   (do (append-log wr l)
                                       (.close  wr)
                                       (.renameTo (io/file path) (io/file (str path ".old")))
                                       (reset! w (gz-writer path))
                                       (reset! i 0))
                                   (do (append-log @w l) ;; TODO: replace @w with wr?
                                       (swap! i inc))))))))


(defn emit [_ {ts :ts ns :ns ev :ev :as l}]
  (doseq [a (vals @appenders)] (a l)))

(defn log [ev arg]
  (when *enable*
    (send-off publisher emit (mk-log ev arg))
    nil))

(defn exeption [ev e & [args]]
  (log ev (assoc (or args {})
                 :lvl :error
                 :msg (.getMessage e)
                 :etr (pr-str e))))

(defn error [ev arg]
  (log ev (assoc arg :lvl :error)))

(defn warn [ev arg]
  (log ev (assoc arg :lvl :warn)))

(defn info [ev arg]
  (log ev (assoc arg :lvl :info)))

(defn debug [ev arg]
  (log ev (assoc arg :lvl :debug)))

(defn trace [ev arg]
  (log ev (assoc arg :lvl :trace)))


(defn parse-int [s]
  (when-let [x (re-matches #"[-+]?\d+" (str s))]
    (Integer/parseInt x)))

(defn save-to-file [pth cont]
  (spit pth cont :append true))

(def ^:const hour 3600000)

(defn es-appender [arg]
  (let [{:keys [lvl
                es-url        es-auth
                index-pat     appender-id
                batch-size    batch-timeout
                fallback-file fallback-max-lines] #_"TODO: add max-lines support"
         :or   {lvl                :all
                appender-id        :es
                index-pat          "'aidbox-logs'-yyyy-MM-dd"
                batch-size         200
                batch-timeout      hour
                fallback-max-lines 10000}}
        (apply dissoc arg (for [[k v] arg :when (nil? v)] k))

        batch-size         (parse-int batch-size)
        batch-timeout      (parse-int batch-timeout)
        fallback-max-lines (parse-int fallback-max-lines)
        es-url             (str es-url "/_bulk")

        post-params   (cond-> {:headers {"Content-Type" "application/x-ndjson"}}
                        es-auth (assoc :basic-auth es-auth))
        date-fmt      (DateTimeFormatter/ofPattern index-pat)
        default-state (fn [] {:start-time (System/currentTimeMillis)
                             :batch      nil
                             :i          0})
        state         (atom (default-state))
        log-fallback  (if fallback-file (partial save-to-file fallback-file) (comp println str/trim-newline))]
    (letfn [(mk-idx      [n] (str "{\"index\": {\"_index\": \"" n "\"}}\n"))
            (get-idx     []  (mk-idx (.format date-fmt (java.time.LocalDateTime/now))))
            (mk-log-line [m] (str (get-idx) (json/generate-string m) "\n"))
            (log-error   [batch l errs]
              (->> (for [{:keys [error status]} errs]
                    (->> {:err  (some-> error .getMessage)
                           :w_st status
                           :etr  (with-out-str (stacktrace/print-stack-trace error))
                           :lvl  :error
                           :ev   :log/ex}
                          (merge (select-keys l [:w :tn :ts :ctx]))
                          mk-log-line))
                   str/join
                   (str batch)
                   log-fallback))
            (report-posting-errors [reporter {:keys [error status] :or {status 500} :as args}]
              (some->> (or (when error
                             [{:status status
                               :error error}])
                           (let [body (some-> (:body args) (json/parse-string keyword))]
                             (when (:errors body)
                               (->> (:items body)
                                    (mapcat
                                     (fn [actions]
                                       (for [[_ {:keys [status error]}] actions
                                             :when (seq error)]
                                         {:status status
                                          :error (Exception. (json/generate-string error))}))))))
                           (when (< 299 status)
                             [{:status status
                               :error (Exception. (str status))}]))
                       reporter))]
      (add-appender
       appender-id
       lvl
       (fn [l]
         (swap! state #(-> % (update :batch str (mk-log-line l))
                           (update :i inc)))
         (when-let [batch (and (or (<= batch-size (:i @state))
                                   (< batch-timeout (- (System/currentTimeMillis) (:start-time @state))))
                               (:batch @state))]
           (try (http/post es-url (assoc post-params :body batch) (partial report-posting-errors (partial log-error batch l)))
                (catch Exception e (log-error batch l [{:error e :status 500}])))
           (reset! state (default-state))))))))



(comment
  (json/parse-string (:body (nth @r 93)) keyword)
  (disable-log)

  (append-log *out* {:ev :myevent})

  *out*

  (es-appender
   {:es-url "http://localhost:9200"
    :appender-id :es
    :batch-size 1
    :batch-timeout 3600})

  (stdout-appender)

  (clear-appenders)
  (obscure-appender "tcp://localhost:7777")


  (file-appender "/tmp/logs" 100000)

  (def w (gz-writer "/tmp/logs"))

  (append-log w {:ev :ups/myevent
                 :ts "2011-01-01"
                 :msg "ups"})

  (log ::event {:lvl :error :msg "Hello"})

  (doseq [i (range 10000)]
    (log ::event {:message (str "msg - " i)}))

  (cheshire.core/generate-stream {:a 1} *out*)

  (def f (io/file "/tmp/logs.ndjson"))

  (int (/ (.length f) 1024.0))

  (.close w)


  (enable-log)

  )
