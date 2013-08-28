(defproject fire.simulate "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:repl-options {:port 64433}}}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.2.0"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  ;:warn-on-reflection true
  :main fire.main)
