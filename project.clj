(defproject hack-assembler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[me.raynes/fs "1.4.6"]
                 [org.apache.commons/commons-lang3 "3.5"]
                 [org.clojure/clojure "1.8.0"]
                 [superstring "2.1.0"]]
  :main ^:skip-aot hack-assembler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
