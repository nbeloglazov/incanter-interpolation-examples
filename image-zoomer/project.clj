(defproject image-zoomer "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-core "1.5.0"]
                 [seesaw "1.4.2" :exclusions
                  [[org.clojure/clojure]]]]
  :resource-paths ["resources"]
  :main image-zoomer.core)
