(defproject particlebox "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]]
  :plugins [[lein-cljsbuild "1.0.1"]
            [com.cemerick/clojurescript.test "0.2.1"]]
  :cljsbuild {
              :builds [{:id :prod
                        :source-paths ["src"]
                        :compiler {
                                   :externs ["externs/three.js"]
                                   :output-to "resources/main.js"
                                   :pretty-print false
                                   :optimizations :advanced
                                   }}
                       {:id :test
                        :source-paths ["src" "test"]
                        :compiler {
                                   :externs ["externs/three.js"]
                                   :output-to "resources/main_test.js"
                                   :optimizations :whitespace
                                   }}]
              :test-commands {"unit-tests"
                              ["phantomjs" :runner
                               "window.literal_js_was_evaluated=true"
                               "externs/three.js"
                               "resources/main_test.js"]}})
