(defproject mini-beast "1.0.0-SNAPSHOT"
  :description "mini-beast"
  :dependencies [[org.clojure/clojure "1.4.0"]
      [overtone "0.7.1"]
      [quil "1.4.1"]
      [commons-collections "3.0-dev2"]]
  :main minibeast.core
  ;;:main overtone.live
  :repl-init minibeast.core
)
