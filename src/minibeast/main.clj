(ns minibeast.main
  (:gen-class))

(defn -main [& args]
  (alter-var-root #'*command-line-args* (constantly args))
  ((requiring-resolve 'minibeast.core/-main)))
