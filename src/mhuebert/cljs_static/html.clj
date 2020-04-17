(ns mhuebert.cljs-static.html
  (:require [hiccup2.core :as hiccup]
            [hiccup.util :as hu]
            [mhuebert.cljs-static.page :as page]))

(defn html-page [root]
  (hiccup/html {:mode :html}
               (hu/raw-string page/doctype)
               (page/root root)))