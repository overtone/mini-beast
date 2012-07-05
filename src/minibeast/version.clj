(ns minibeast.version)

(def BEAST-VERSION {:major 0
                    :minor 0
                    :patch 1
                    :snapshot true})

(def BEAST-VERSION-STR
  (let [version BEAST-VERSION]
    (str "v"
         (:major version)
         "."
         (:minor version)
         (if-not (= 0 (:patch version)) (str "." (:patch version)) "")
         (if (:snapshot version) "-dev" ""))))
