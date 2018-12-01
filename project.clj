(defproject lox "0.1.0-SNAPSHOT"
  :description "Lox language impentation in lox"
  :url "https://github.com/RobinVdBroeck/lox-clojure"
  :license {:name "MIT"
            :url "https://github.com/RobinVdBroeck/lox-clojure/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot lox.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
