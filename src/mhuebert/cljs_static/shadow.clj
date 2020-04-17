(ns mhuebert.cljs-static.shadow
  (:require [mhuebert.cljs-static.assets :as a]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.tools.reader.edn :as edn]
            [com.stuartsierra.dependency :as dep]
            [me.raynes.fs :as fs]
            [shadow.cljs.devtools.config :as config]))

(defn get-build [id]
  (config/get-build! id))

(defmacro with-shadow-state [build-state & body]
  `(let [build-state# ~build-state]
     (do ~@body)
     build-state#))

(defn eval-if-fn [f] (if (fn? f) (f) f))

(defn- resolve-sym [sym]
  (require (symbol (namespace sym)))
  (resolve sym))

(defn- resolve-content [form]
  (cond (string? form) form
        (symbol? form) (eval-if-fn @(resolve-sym form))
        (list? form) (let [[f-sym & args] form]
                       (apply @(resolve-sym f-sym) args))
        :else (throw (ex-info "Content must be a string, symbol, or simple function call."
                              {:form form}))))

(defn write-assets!
  "Writes assets to the output-dir of the current shadow build.

   `assets` should be a map of {<path>, <form>}, where `form` will be evaluated.

   Intended for use in a shadow-cljs.edn config file."
  {:shadow.build/stage :flush}
  [build-state assets]
  (if (and (get-in build-state [::generated assets])
           (not (:always? assets)))
    build-state
    (with-shadow-state build-state
      (binding [a/*public-path* (:public-path assets)]
        (doseq [[path content] (dissoc assets :always? :public-path)]
          (a/write-asset! path (resolve-content content))))
      (-> build-state
          (assoc-in [::generated assets] true)))))

(defn transitive-module-deps
  "Give shadow module config, return sorted transitive dependencies, including ks."
  [module-ks modules]
  (let [graph (reduce-kv (fn [graph k {:keys [depends-on]}]
                           (reduce #(dep/depend %1 k %2) graph depends-on))
                         (dep/graph) modules)
        deps (mapcat (partial dep/transitive-dependencies graph) module-ks)]
    (sort (dep/topo-comparator graph) (set (into module-ks deps)))))

(defn read-manifest [{:as build
                      :keys [output-dir]}]
  (edn/read-string (slurp (str output-dir "/manifest.edn"))))

(def module-index
  (memoize
    (fn [manifest]
      (->> manifest
           (reduce (fn [m {:keys [module-id output-name]}]
                     (assoc m module-id output-name)) {})))))

(defn module-path [build-id module-k]
  "Reads module path for `module-k` from shadow-cljs manifest.edn"
  (let [{:as build
         :keys [asset-path]} (get-build build-id)
        index (module-index (read-manifest build))]
    (str asset-path "/" (or (index module-k)
                            (throw (ex-info "Module not found" {:key module-k}))))))

(defn module
  "Script tag for module `k`"
  [build-id k]
  [:script {:src (module-path build-id k)}])

(defn modules
  "Sorted list of script tags for modules `ks` and their transitive dependencies."
  [build-id ks & {:keys [exclude]}]
  {:pre [(keyword? build-id) (vector? ks)]}
  (->> (cond->> (transitive-module-deps ks (:modules (config/get-build! build-id)))
                exclude (remove exclude))
       (map (partial module build-id))))

(defn js-call
  "emit js code for simple calls like '(some-fn \"arg1\")"
  [[sym & args]]
  (str (-> (str sym)
           (str/replace "/" ".")
           (munge))
       "(" (str/join ", " (map json/write-str args)) ")"
       ";"))

(defn copy
  {:shadow.build/stages #{:configure
                          :compile-prepare
                          :compile-finish
                          :optimize-prepare
                          :optimize-finish
                          :flush}}
  [state {:as directories
          :keys [shadow.build/stage]
          :or {stage :flush}}]
  (when (= stage (:shadow.build/stage state))
    (doseq [[from to] (dissoc directories :shadow.build/stage)
            :when (fs/exists? from)]
      (if (fs/directory? from)
        (fs/copy-dir-into from to)
        (fs/copy+ from to))))
  state)

(defn exec* [state cmd-or-opts]
  (let [{:keys [shadow.build/stage
                shadow.build/mode
                cmd]} (merge {:shadow.build/stage :flush
                              :shadow.build/mode :always}
                             (cond->> cmd-or-opts
                                      (not (map? cmd-or-opts))
                                      (hash-map :cmd)))]
    (when (and (= (:shadow.build/stage state) stage)
               (or (= mode :always)
                   (= mode (:shadow.build/mode state))))
      (apply sh (str/split cmd #"\s+")))))

(defn exec
  {:shadow.build/stages #{:configure
                          :compile-prepare
                          :compile-finish
                          :optimize-prepare
                          :optimize-finish
                          :flush}}
  [state & cmds]
  (doseq [cmd cmds]
    (exec* state cmd))
  state)
