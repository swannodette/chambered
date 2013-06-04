(defproject chambered-cljs "0.1.0-SNAPSHOT"
  :description "A port of Notch's Chambered JavaScript demo"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]

  :source-paths ["src"]

  :plugins [[lein-cljsbuild "0.3.2"]]

  :cljsbuild {:build
               [{:source-path ["src"]
                 :id "chambered"
                 :compiler {:output-to "chambered.js"
                            :static-fns true
                            :pretty-print true}}]})
