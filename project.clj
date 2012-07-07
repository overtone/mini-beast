(defproject mini-beast "0.0.1-SNAPSHOT"
  :description "mini-beast"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [overtone "0.8.0-SNAPSHOT"]
                 [quil "1.6.0"]
                 [commons-collections "3.0-dev2"]]
  :main ^{:skip-aot true} minibeast.core
  :repl-init minibeast.core)
