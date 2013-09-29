(ns hugo-a-go-go.server
  (:use [compojure.core]
        [hiccup.core])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :refer [header]]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:gen-class))

(defn page-for-env [env]
  (html [:head {:title "test"}
         [:link {:rel "stylesheet" :href "css/style.css"}]
         [:script {:src (str "js/" env ".js")}]
         ]
        [:body
         {:onload "hugo_a_go_go.client.init()"
          :onclick "javascript: hugo_a_go_go.play.handle(event);"}
         [:p]
         [:div [:canvas#board {:width "600" :height "600"}]]
         ]))

(defroutes site-routes
  (GET "/" []
       (page-for-env "prod"))
  (GET "/dev" []
       (page-for-env "dev"))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app (handler/site site-routes))
