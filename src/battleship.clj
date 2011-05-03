(ns battleship
  (:gen-class)
  (:use [battleship.core :only [core-app]]
        [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware
         [file :only [wrap-file]]
         [file-info :only [wrap-file-info]]
         [stacktrace :only [wrap-stacktrace]]]))

(defn run-server [& args]
  (let [[port db-name] args]
    (run-jetty (-> #'core-app
                   (wrap-file "static")
                   wrap-file-info
                   wrap-stacktrace)
               {:port (Integer. (or port 8081))
                :join? false})))

(defn -main [& args]
  (apply run-server args))

