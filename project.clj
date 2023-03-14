(defproject helpshift/tardigrade "0.1.0-SNAPSHOT"
  :description "A library of resiliency utilties"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-time "0.9.0"]
                 [org.clojure/core.async "1.6.673"]]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]]}})
