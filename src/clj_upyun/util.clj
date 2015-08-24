(ns clj-upyun.util
  (require [clojure.data.codec.base64 :as b64])
  (import [java.security MessageDigest]
          [java.math BigInteger]
          [java.util UUID]
          [java.net URLEncoder URLDecoder]))

(defn uuid
  "Generate uuid."
  []
  (str (UUID/randomUUID)))

(defn md5
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn base64-encode
  [s]
  (-> (.getBytes s "UTF-8")
      b64/encode
      (String.)))

(defn extension
  "Get file extension."
  [file-name]
  (if-let [i (.lastIndexOf file-name ".")]
    (subs file-name i)))

(defn url-encode
  [string]
  (some-> string str (URLEncoder/encode "UTF-8") (.replace "+" "%20")))
