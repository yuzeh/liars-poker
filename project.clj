(defproject liars-poker "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/tools.cli "0.2.4"]
                 [bigml/sampling "2.1.0"]
                 ]
  :main ^:skip-aot liars-poker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
