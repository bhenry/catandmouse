(ns battleship.core
  (:use [net.cgrand.moustache :only [app delegate]]
        [karras.core :only [with-mongo-request mongo-db]]
        [ring.middleware
         [params :only [wrap-params]]
         [session :only [wrap-session]]
         [keyword-params :only [wrap-keyword-params]]])
  (:require [battleship.game :as game]
            [battleship.views :as views]))

(defn wrap-server-error [handler]
  (fn [req]
    (try (handler req)
         (catch Exception e
           (views/server-error req)))))

(defn wrap-auth [handler]
  (fn [req]
    (handler req)))

(defn with-mongo-db [handler db-name]
  (let [db (mongo-db db-name)]
    (fn [request]
      (with-mongo-request db
        (handler request)))))

(def routes
     (app
      [] views/new
      ["wait"] views/wait
      ["join"] views/join
      ["game"] views/gameview
      ["move"] views/move
      [& _] views/not-found))

(def core-app
     (-> #'routes
         (wrap-auth)
         (with-mongo-db :cat-and-mouse)
         (wrap-params)
         (wrap-keyword-params)
         (wrap-session {:cookie-name "battleboats"})))
