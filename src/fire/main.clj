(ns fire.main
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.core.typed :refer [ann check-ns print-env letfn> typed-deps
                                        ann-protocol loop> def-alias dotimes> ann-form]]
            [clojure.core.typed.hole :refer [silent-hole]]
            [clojure.tools.analyzer.hygienic :as hy]
            [clojure.java.io :refer [reader]]
            [clojure.string :as str]
            [fire.gnuplot :as plot]
            [fire.simulate
             [percolation :as perc]])
  (:import (clojure.lang Seqable))
  (:gen-class))

(typed-deps fire.types
            fire.gnuplot
            fire.simulate
            fire.simulate.percolation
            clojure.core.typed.hole)

(def-alias RunOptions
  '{:size Number
    :q Number
    :p Number
    :f Number
    :l Long})

(ann run-percolation [RunOptions -> nil])
(defn run-percolation [{:keys [size q p f l]}]
  (let [proc (ann-form
               (atom (perc/start! :grid (perc/initial-grid
                                          :q q :p p :f f :rows size
                                          :cols size)))
               (Atom1 perc/PercolationP))]
    (dotimes> [_ l]
      (swap! proc perc/next!))))

(ann -main [String * -> nil])
(defn -main 
  "Main entry point."
  [& args]
  (let [[options args banner]
        (cli args
             ["--size" "The size of the square lattice" :default 100, :flag false, :parse-fn #(Long/parseLong %)]
             ["-q" "Probability of generating a tree in starting grid." :default 0.5, :flag false, :parse-fn #(Double/parseDouble %)]
             ["-p" "Probability of tree regrowth" :default 0.5, :flag false, :parse-fn #(Double/parseDouble %)]
             ["-f" "Probability of lightning" :default 0.1, :flag false, :parse-fn #(Double/parseDouble %)]
             ["-l" "The number of iterations" :default 50, :flag false, :parse-fn #(Long/parseLong %)]
             ["-h" "--help" "Display usage." :default false, :flag true])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (run-percolation options)
    (println "Simulation over. Exiting in 30 seconds...")
    (Thread/sleep 30000)))
