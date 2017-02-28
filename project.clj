(defproject tomthought/palette "0.1.2"
  :description "A Clojure[script] library to manipulate RGB[A] and HEX colors."
  :url "https://github.com/tomthought/palette"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.494"]

                 [com.7theta/utilis "0.8.3"]]

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-cljsbuild "1.1.5"]]

  :min-lein-version "2.5.3"

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :figwheel {:server-port 3449 ;; default
             :open-file-command "emacsclient"
             :nrepl-middleware ["cider.nrepl/cider-middleware"]}

  :profiles
  {:dev
   {:dependencies [[figwheel-sidecar "0.5.9"]
                   [com.cemerick/piggieback "0.2.1"]
                   [org.clojure/tools.nrepl "0.2.12"]
                   [org.clojure/tools.namespace "0.2.11"]]
    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
    :plugins      [[lein-figwheel "0.5.9"]]
    :source-paths ["dev" "src"]
    :figwheel {:http-server-root "public" ;; default and assumes "resources"
               :server-port 3449 ;; default
               :css-dirs ["public/css"]
               :open-file-command "emacsclient"
               :nrepl-middleware ["cider.nrepl/cider-middleware"]}
    }}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src"]
     :compiler     {:main                 palette.core
                    :output-to            "resources/public/js/compiled/app.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    }
     :figwheel {:websocket-host "localhost"
                :auto-jump-to-source-on-error true}}

    {:id           "min"
     :source-paths ["src"]
     :compiler     {:main            palette.core
                    :output-to       "resources/public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}


    ]}

  :scm {:name "git"
        :url "https://github.com/tomthought/palette"}

  )
