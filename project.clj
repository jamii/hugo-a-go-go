(defproject hugo-a-go-go "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ring "1.2.0"]
                 [org.clojure/core.async "0.1.222.0-83d0c2-alpha"]
                 [org.clojure/clojurescript "0.0-1909"]]
  :plugins [[lein-cljsbuild "0.3.3"]
            [lein-ring "0.8.7"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {:builds {:dev
                       {:source-paths ["src/brepl" "src/cljs"]
                        :compiler {:output-to "resources/public/js/dev.js"
                                   :optimizations :simple
                                   :pretty-print true
                                   :static-fns true}}

                       :prod
                       {:source-paths ["src/cljs"]
                        :compiler {
                                   ;; :output-to "prod.js"
                                   ;; :output-dir "resources/public/js/"
                                   :output-to "resources/public/js/prod.js"

                                   :optimizations :advanced
                                   :pretty-print true
                                   ;; :source-map "resources/public/js/prod.js.map"
                                   :static-fns true
                                   }}
                       }}
  :main hugo-a-go-go.server
  :ring {:handler hugo-a-go-go.server/app})
