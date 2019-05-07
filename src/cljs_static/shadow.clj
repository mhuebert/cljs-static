(ns cljs-static.shadow
  (:require [cljs-static.assets :as a]))

(defmacro with-shadow-state [build-state & body]
  `(let [build-state# ~build-state]
     (binding [a/*content-hashes?* (= :release (:shadow.build/mode build-state#))]
       (do ~@body)
       build-state#)))

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