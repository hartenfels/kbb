(defproject kbb "0.3.0-SNAPSHOT"
  :description "Kanban Board"
  :url "https://github.com/hartenfels/kbb"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.cli "0.4.2"]]
  :main ^:skip-aot kbb.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
