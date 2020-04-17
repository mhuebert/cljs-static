(ns mhuebert.cljs-static
  (:require [mhuebert.cljs-static.html :as html]
            [mhuebert.cljs-static.shadow :as shadow]))

(def ^{:shadow.build/stage :flush} assets! shadow/write-assets!)
(def html html/html-page)