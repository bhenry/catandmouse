(defproject skipbo "0.0.1"
  :description "skipbo on the web"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [hiccup "0.3.1"]
                 [net.cgrand/moustache "1.0.0-SNAPSHOT"]
                 [pour "0.1.0"]
;;                 [karras "0.5.0"]
                 [org.clojars.sethtrain/karras "0.6.0"]
                 [ring/ring-servlet "0.3.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [ring/ring-jetty-adapter "0.3.1"]
                     [ring/ring-devel "0.3.1"]
                     [lein-search "0.3.3"]
                     [uk.org.alienscience/leiningen-war "0.0.13"]]
  :aot [skipbo]
  :war {:name "skipbo.war"})

