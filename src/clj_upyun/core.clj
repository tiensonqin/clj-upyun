(ns clj-upyun.core
  (:require [clj-upyun.util :refer [uuid md5 base64-encode extension url-encode]]
            [cheshire.core :refer [generate-string]]
            [clj-time.core :as time]
            [clj-time.format :as fmt]
            [clojure.java.io :as io]
            [clojure.java.io :refer [input-stream file]]
            [clojure.string :as s]
            [org.httpkit.client :as http]))

(defonce ^:private api-uri "http://v0.api.upyun.com")

(defn build-path
  [& args]
  (let [args (filter some? args)
        tails (s/join "/" args)]
    (if (re-find #"^http" (first args))
      tails
      (str "/" tails))))

(defn policy&sign
  [args form-api-secret]
  (let [policy (some-> args
                       generate-string
                       base64-encode)
        sign (-> policy (str "&" form-api-secret) md5)]
    {:policy policy
     :signature sign}))

(defn sign
  [method path date content-length password]
  (let [md5-password (md5 password)]
    (-> (s/join "&" [method path date content-length md5-password])
        md5)))

(defn notify-sign-legal?
  ([sign code message url time api-secret]
   (= sign
      (->> [code message url time api-secret]
           (s/join "&")
           md5)))
  ([sign code message url time api-secret ext-param]
   (= sign
      (->> [code message url time api-secret ext-param]
           (s/join "&")
           md5))))

(defn gmt-date
  []
  (fmt/unparse (fmt/formatter "EEE, dd MMM yyyy HH:mm:ss 'GMT'") (time/now)))

(defprotocol ICdn
  (getAuthorization [_ path method content-length])
  (ls [_] [_ path])
  (uploadFile [_ path options])
  (uploadLocalFile [_ local-path cloud-path options])
  (putFile [_ path content])
  (deleteFile [_ file])
  (getFile [_ file])
  ;; Dangerous
  (deleteBucketDangerous [_]))

(defn delete-recur
  ([this]
   (delete-recur this nil))
  ([this path]
   (while (some? (.ls this path))
     (when-let [files (.ls this path)]
       (doseq [{:keys [file type]} files]
         (if-not (= "F" type)
           (.deleteFile this (if path (str path "/" file) file))
           (delete-recur this (if path (str path "/" file) file))))
       (if path (.deleteFile this path))))))

(defrecord Upyun [bucket manager password]
  ICdn
  (getAuthorization [_ path method content-length]
    (let [date (gmt-date)
          headers {"Authorization" (str "UpYun " manager ":" (sign method path date content-length password))
                   "Date" date}]
      headers))

  (ls [this]
    (ls this nil))

  (ls [this path]
    (let [body (-> @(http/get (build-path api-uri bucket path)
                              {:headers (getAuthorization this (build-path bucket path) "GET" 0)})
                   :body)]
      (when (not= "" body)
        (some->>
         (s/split body #"\n")
         (map
           #(s/split % #"\t"))
         (map (partial zipmap [:file :type :size :last-modified]))))))

  (uploadFile [this [tempfile filename] options]
    (let [rand-name (str (md5 (uuid)) (extension filename))
          headers (-> (getAuthorization this (build-path bucket rand-name) "PUT" (.length tempfile))
                      (merge options))
          result @(http/put (build-path api-uri bucket rand-name)
                            {:headers headers
                             :body (input-stream tempfile)})]
      (if-let [{:keys [x-upyun-width x-upyun-height x-upyun-frames x-upyun-file-type]} (:headers result)]
        {:ok {:width x-upyun-width
              :height x-upyun-height
              :frames x-upyun-frames
              :type x-upyun-file-type
              :url rand-name}}
        {:error (:body result)})))

  (uploadLocalFile [this local-path cloud-name options]
    (let [file (io/file local-path)
          cloud-path (str cloud-name (extension local-path))
          _ (prn "cloud path: " cloud-path)
          headers (-> (getAuthorization this (build-path bucket cloud-path) "PUT" (.length file))
                      (merge options))
          result @(http/put (build-path api-uri bucket cloud-path)
                            {:headers headers
                             :body (input-stream file)})
          _ (prn result)]
      (if-let [{:keys [x-upyun-width x-upyun-height x-upyun-frames x-upyun-file-type]} (:headers result)]
        {:ok {:width x-upyun-width
              :height x-upyun-height
              :frames x-upyun-frames
              :type x-upyun-file-type
              :url cloud-path}}
        {:error (:body result)})))

  (putFile [this path content]
    (let [headers (getAuthorization this (build-path bucket path) "PUT" (count content))
          result @(http/put (build-path api-uri bucket path)
                            {:headers headers
                             :body (input-stream content)})]
      (if-let [{:keys [x-upyun-width x-upyun-height x-upyun-frames x-upyun-file-type]} (:headers result)]
        {:ok {:width x-upyun-width
              :height x-upyun-height
              :frames x-upyun-frames
              :type x-upyun-file-type
              :url path}}
        {:error (:body result)})))

  (deleteFile [this file]
    (let [file (url-encode file)
          result @(http/delete (build-path api-uri bucket file)
                               {:headers (getAuthorization this (build-path bucket file) "DELETE" 0)})
          {status :status body :body} result]
      (if (= status 200)
        {:ok nil}
        {:error (:body result)})))

  (getFile [this file]
    (let [file (url-encode file)
          result @(http/get (build-path api-uri bucket file)
                            {:headers (getAuthorization this (build-path bucket file) "GET" 0)
                             :as :byte-array})
          {status :status body :body} result]
      (if (= status 200)
        {:ok body}
        {:error (:body result)})))

  (deleteBucketDangerous [this]
    (delete-recur this)))
