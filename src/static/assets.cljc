(ns static.assets
  (:require [clojure.string :as str]
            #?@(:clj  [[clojure.java.io :as io]
                       [me.raynes.fs :as fs]]
                :cljs [["md5" :as md5-fn]
                       ["fs" :as fs]
                       ["mkdirp" :as mkdirp]
                       ["path" :as path]]))
  #?(:clj (:import (java.security MessageDigest))))

(def join-paths #?(:clj  io/file
                   :cljs path/join))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Asset handling

;; Dynamic vars are used for asset-path options

(def ^:dynamic *public-path* nil)

(def ^:dynamic *content-hashes?*
  "When true, append content-hashes to asset paths."
  false)

(def ^:dynamic *asset-host*
  "Host where assets are to be accessed by client"
  nil)

(defn strip-slash [path]
  (cond-> path
          (str/starts-with? path "/") (subs 1)))

(def md5
  "Returns md5 hash for string"
  ;; from https://gist.github.com/jizhang/4325757#gistcomment-2196746

  #?(:clj  (fn [s] (let [algorithm (MessageDigest/getInstance "MD5")
                         raw (.digest algorithm (.getBytes s))]
                     (format "%032x" (BigInteger. 1 raw))))
     :cljs md5-fn))

(defn try-slurp [file]
  (try #?(:clj  (slurp file)
          :cljs (some-> (fs/readFileSync file) (str)))
       (catch #?(:clj  Exception
                 :cljs js/Error) e nil)))

(def make-parents #?(:clj  io/make-parents
                     :cljs (fn [s]
                               (mkdirp (str/replace s #"/[^/]+$" "")))))

(defn asset-file [path]
  (assert *public-path* "*public-path* must be set")
  (join-paths *public-path* (strip-slash path)))

(defn read-asset
  "Returns the contents for an asset"
  [path]
  (-> (asset-file path)
      (try-slurp)))

(def write!
  #?(:clj  spit
     :cljs fs/writeFileSync))

(defn asset-path
  "Asset-path function, for use in generating HTML"
  [path]
  (if-not (str/starts-with? path "/")
    path
    (let [prefix *asset-host*
          postfix (when *content-hashes?*
                    (some->> (read-asset path)
                             (md5)
                             (str "?v=")))]
      (str prefix path postfix))))

(defn write-asset!
  "Write `content` string to an asset file"
  [path content]
  (doto (asset-file path)
    (make-parents)
    (write! content))
  (println (str " + " path)))

#?(:clj
   (defn copy-dir!
     "Copy directories"
     {:shadow.build/stage :flush}
     [build-state directories]
     (doseq [[from to] directories]
       (fs/copy-dir from to))
     build-state))