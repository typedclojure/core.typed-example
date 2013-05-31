(ns fire.simulate.percolation
  "This namespaces defines the initial grid function for
  the percolation problem"
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst into-array>
                                        override-method Atom1 letfn> ann-form dotimes>
                                        nilable-param non-nil-return]
             :as tc]
            [clojure.math.numeric-tower :refer [floor abs]]
            [clojure.tools.analyzer.hygienic :refer [macroexpand]]
            [clojure.reflect :as reflect]
            [fire.gnuplot :as plot]
            [fire.simulate :as sim]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.trace :as trace])
  (:import (clojure.lang IPersistentSet IPersistentVector Seqable)
           (java.io File)))

(typed-deps fire.simulate 
            fire.gnuplot)

;--------------------------------------------------
; Types
;--------------------------------------------------

(def-alias PercolationP
  "The state of a percolation run.

  - :gnuplot  the actual gnuplot process
  - :grid     a grid used to update gnuplot"
  '{:gnuplot plot/GnuplotP,
    :grid sim/Grid})

;--------------------------------------------------
; Grid operations
;--------------------------------------------------

(def-alias PatchShape
  "The shape of the initial burning patch of the grid"
  (U ':square ':line ':rectangle))

(ann initial-grid [& {:rows Long, :cols Long :q Number :f Number :p Number :wind sim/Wind
                      :patch-shape PatchShape} -> sim/Grid])
(defn initial-grid
  "Return the initial grid. States are :tree (green tree) with probability q, and :empty
  (empty point) with probability 1 - q.
  Ignites a small patch of trees in the centre of the grid.
  If not provided, number of rows/columns default to 100.
  q defaults to 0.1."
  [& {:keys [wind rows cols q f p patch-shape] :or {rows 100, cols 100, q 0.1, f 0, p 0, wind :none,
                                                    patch-shape :square}}]
  (letfn> [burning? :- [sim/Point -> Boolean]
           (burning? 
             ;Ignite a small patch of trees in the centre.
             [[row col]]
             (let [mid-row (floor (/ rows 2))
                   mid-col (floor (/ cols 2))]
               (boolean
                 (when (and (<= 0 mid-row)
                            (<= 0 mid-col))
                   (and (<= (abs (- row mid-row)) 2)
                        (<= (abs (- col mid-col)) 2))))))

           gen-state :- [sim/Point -> sim/State]
           (gen-state
             ;State is a :tree with probability q, :empty
             ;with probability 1-q, or :burning if part of the
             ;small patch of trees in the centre of the grid.
             [pnt]
             (cond 
               (burning? pnt) :burning
               (sim/occurs? q) :tree 
               :else :empty))]
    (sim/grid-from-fn gen-state :rows rows, :cols cols,
                      :q q :f f :p p :wind wind)))

;--------------------------------------------------
; gnuplot ops
;--------------------------------------------------

(ann start! [& {:q Number, :grid sim/Grid, :p Number, :f Number :no-update Any, :gnuplot plot/GnuplotP :wind sim/Wind} -> PercolationP])
(defn start! 
  "Start a gnuplot process and initialize it to the starting
  state. 

  Keyword arguments:
  - :grid  an optional initial grid instead of generating one
  - :q     the :q passed to initial-grid.
  - :p     the probability of a tree growing in an empty square.
  - :f     the probability of lightning.
  - :wind  wind direction
  - :no-update  If true, do not update a graph with the initial state. Defaults to nil.
  - :gnuplot  an optional gnuplot process

  Returns a map for percolation ops."
  [& {:keys [q p f grid no-update gnuplot wind] :or {q 0.1, p 0, f 0, wind :none}}]
  (let [grid0 (or grid (initial-grid :q q, :f f, :p p :wind wind))
        proc (or gnuplot (plot/start))]
    (sim/with-gunplot-toplevel proc
      (sim/setup-gnuplot! proc)
      (when-not no-update
        (sim/update-simulation! proc grid0)))
    {:gnuplot proc,
     :grid grid0}))

(ann next! [PercolationP & {:no-update Any} -> PercolationP])
(defn next!
  "Calculate the next state for the percolation plot, and update
  the gnuplot process. Returns a new map for percolation ops.
  
  Options
  - :no-update  If true, do not plot the next grid. Defaults to nil."
  [{grid0 :grid, :keys [gnuplot] :as in-grid} & {:keys [no-update]}]
  (let [grid1 (sim/next-grid grid0)]
    (when-not no-update
      (sim/with-gunplot-toplevel gnuplot
        (sim/update-simulation! gnuplot grid1)))
    (-> in-grid
        (assoc :grid grid1))))

(ann set-eps String)
(def set-eps "set terminal postscript eps enhanced")

(ann set-epslatex String)
(def set-epslatex "set terminal epslatex")

(ann flush-epslatex [-> nil])
(defn flush-epslatex 
  "Sometimes we need to change the output terminal to
  help epslatex output a full tex file. This makes
  'output' and 'terminal' dirty, they should be set again."
  []
  (println "set output")
  (flush))

(ann plot-forest-to-eps [PercolationP String -> nil])
(defn plot-forest-to-eps
  "Plot a forest grid to eps, to the specified path (extension omitted)"
  [{:keys [gnuplot grid]} path-no-ext]
  (println set-epslatex)
  (println (str "set output '" path-no-ext ".tex'"))
  (sim/plot-forest gnuplot grid)
  (flush-epslatex))

(ann ^:nocheck clojure.core/replace 
     (All [y] [(clojure.lang.IPersistentMap Any y) (clojure.lang.Seqable y) 
               -> (clojure.lang.Seqable y)]))

(ann update-without-plot (Fn [PercolationP -> PercolationP]
                             [PercolationP AnyInteger -> PercolationP]))
(defn- update-without-plot
  ([p] (update-without-plot p 1))
  ([p n]
   {:pre [(integer? n)]}
   (if (zero? n)
     p
     (recur (next! p :no-update true) (dec n)))))

(ann run-percolation-test [String AnyInteger -> nil])
(defn run-percolation-test
  "Takes a string naming the output folder, and the
  value of q, and outputs the graphs needed for the percolation test"
  [folder q]
  (letfn> [update-without-plot :- [PercolationP -> PercolationP]
           (update-without-plot [p]
             (next! p :no-update true))

           path-no-ext :- [String -> String]
           (path-no-ext [s]
             (str s "-q" (apply str (replace {\. \-} (str q)))))]
    (let [; calculate the initial grid
          initial-state (start! :q q :p 0 :f 0 :no-update true)

          ; calculate the final grid after 100 interations
          {final-grid :grid,
           :as final-state} (last (take 100 (iterate update-without-plot initial-state)))
          _ (assert final-state)
          _ (assert final-grid)

          gp (:gnuplot final-state)]
      (sim/with-gunplot-toplevel gp

        ;; output initial graph
        (let [nme-no-ext (path-no-ext "initial-state")]
          (plot-forest-to-eps initial-state nme-no-ext)
          (flush-epslatex))

        ;; output nburning graph
        (do
          (println set-epslatex "color" "size 3,3")
          (println (str "set output '" (path-no-ext "nburning") ".tex'"))
          (sim/plot-nburning-graph gp final-grid)
          (flush-epslatex))

        ;; output ntree graph
        (do
          (println set-epslatex "color" "size 3,3")
          (println (str "set output '" (path-no-ext "ntree") ".tex'"))
          (sim/plot-ntrees-graph gp final-grid)
          (flush-epslatex))))))

(ann never-nil (All [x] [(U x nil) -> x]))
(defn never-nil [a]
  (assert (not (nil? a)) "Found nil")
  a)

(ann ^:nocheck clojure.core/spit [clojure.java.io/IOFactory Any & {:append Any} -> Any])

(nilable-param java.io.File/createTempFile {2 #{1}})
(non-nil-return java.io.File/createTempFile :all)

(ann multi-plot-nburning-percolation [plot/GnuplotP (Seqable PercolationP) String & 
                                      {} :mandatory {:report-on (IPersistentSet (U ':p ':f ':q ':size))} -> nil])
(defn multi-plot-nburning-percolation 
  "Make a nice multiplot to study relationship of q and grid size
  to fire percolation. Output files are prefixed with (str prefix-str \"-\")
  if non-empty."
  [gp ps prefix-str & {:keys [report-on]}]
  ;; preconditions
  (assert (set? report-on) "Must provide a reporting set")
  (assert (seq ps) "Must provide at least one grid")
  (let [get-history (fn> [p :- PercolationP] 
                         (-> p :grid :history 
                             (ann-form sim/GridHistory)
                             count))]
    (assert (apply == (get-history (first ps))
                   (map get-history (next ps)))
            "All grid histories must be identical length"))

  (let [;; Some temporary files to dump our data
        prefix-str (if (seq prefix-str)
                     (str prefix-str "-")
                     "")
        temp-nburning-file (File/createTempFile (str prefix-str "multiplot-nburning-data") nil)
        temp-ntrees-file (File/createTempFile (str prefix-str "multiplot-ntrees-data") nil)
        temp-nempty-file (File/createTempFile (str prefix-str "multiplot-nempty-data") nil)

        ;; We already have some data about the history of each
        ;; grid stored in advance.
        history-count (-> ps first :grid :history 
                          (ann-form sim/GridHistory)
                          count)
        histories (map (fn> [p :- PercolationP]
                         (-> p :grid :history))
                       ps)
        _ (ann-form histories (Seqable sim/GridHistory))]
    (letfn> [dump-history-fn-to :- [[sim/GridHistoryEntry -> Number] File -> nil]
             (dump-history-fn-to 
               ;; dump the result of applying lfn to a grid's history to the give temp-file
               [lfn temp-file]
               (dotimes> [n history-count]
                 (let [nburnings (map (fn> [e :- sim/GridHistory]
                                        (lfn (nth e n)))
                                       histories)]
                   (ann-form nburnings (Seqable Number))
                   (spit temp-file (str (str/join " " (cons n nburnings)) "\n") :append true))))]
      ;--------------------------------------------
      ;1. dump data to a temp file for all plots
      (dump-history-fn-to (fn> [e :- sim/GridHistoryEntry] (:nburning e)) temp-nburning-file)
      (dump-history-fn-to (fn> [e :- sim/GridHistoryEntry] (:ntrees e)) temp-ntrees-file)
      (dump-history-fn-to (fn> [e :- sim/GridHistoryEntry] (:nempty e)) temp-nempty-file))
      ;--------------------------------------------

    (let [;; plot-opts is everything to the right of `plot 'output.foo' ...`, which is the same for each call
          make-title (ann-form
                       (fn [rows cols q p f]
                         (str (if (< 2 (count report-on))
                                "\\tiny "
                                "\\small ")
                            (str/join ", "
                                      (remove nil?
                                              [(when (report-on :q)
                                                 (str "\\emph{q}=" q))
                                               (when (report-on :p)
                                                 (str "\\emph{p}=" p))
                                               (when (report-on :f)
                                                 (str "\\emph{f}=" f))
                                               (when (report-on :size)
                                                 (str rows "x" cols))]))))
                       [Number Number Number Number Number -> String])
          plot-opts (str/join ", '' " ; '' is reusing the output (temp file) from previous calls
                              (let [indexed-ps (map-indexed (inst vector Number PercolationP Any Any Any Any) ps)]
                                (for> :- String
                                      [[n {{:keys [rows cols q p f]} :grid}] :- '[Number PercolationP], indexed-ps]
                                      (do 
                                        (ann-form [q rows cols] '[Number Number Number])
                                        (assert (number? q))
                                        (assert (number? rows))
                                        (assert (number? cols))
                                        (str "using 1:" (+ 2 n)
                                             " title '"
                                             (make-title rows cols q p f)
                                             "' with lines"
                                             )))))

          ;; the actual gnuplot `plot` command strings for each plot
          nburning-plot-string (let [plot-string (str "plot '" (.getAbsolutePath temp-nburning-file) "' " plot-opts)]
                                 (prn plot-string)
                                 plot-string)
          ntrees-plot-string (let [plot-string (str "plot '" (.getAbsolutePath temp-ntrees-file) "' " plot-opts)]
                               (prn plot-string)
                               plot-string)
          nempty-plot-string (let [plot-string (str "plot '" (.getAbsolutePath temp-nempty-file) "' " plot-opts)]
                               (prn plot-string)
                               plot-string)]
      ;--------------------------------------------
      ;; 2. Generate the plots via epslatex
      (sim/with-gunplot-toplevel gp
        ;; plot nburning graph
          (println set-epslatex "color")
          (println (str "set output '" prefix-str "multi-nburning.tex'"))
          (println "set title 'Burning trees over time'")
          (println "set xlabel 'Time'")
          (println "set ylabel 'Burning trees'")
          (println "set xtics nomirror autofreq")
          (println "set ytics autofreq")
        (println "set size ratio -1.5")
          (println "set key outside right")
          (println "unset x2tics")
          (println "unset y2tics")
;          (doseq> [{{:keys [rows cols]} :grid} :- PercolationP, ps]
;            (println (str "set y2tics add ('" rows "x" cols "' " (* rows cols) ")")))
          (println (str "save '" prefix-str "multigraph-nburning.gp'"))
          (println nburning-plot-string)
          (sim/gnuplot-eof gp)
          (flush-epslatex)

        ;; plot ntrees graph
          (println (str "set output '" prefix-str "multi-ntrees.tex'"))
          (println "set title 'Green trees over time'")
          (println "set ylabel 'Green trees'")
          (println (str "save '" prefix-str "multigraph-ntrees.gp'"))
          (println ntrees-plot-string)
          (sim/gnuplot-eof gp)
          (flush-epslatex)

        ;; plot nempty graph
          (println (str "set output '" prefix-str "multi-nempty.tex'"))
          (println "set title 'Empty points over time'")
          (println "set ylabel 'Empty points'")
          (println (str "save '" prefix-str "multigraph-nempty.gp'"))
          (println nempty-plot-string)
          (sim/gnuplot-eof gp)
          (flush-epslatex)
          (flush)
      ;--------------------------------------------
          ))))

(def-alias MultiPlotEntry
  "Map shorthands for generating grids with make-grids
  
  - :size  make a grid with dimensions size x size
  - :q     probably of points being a tree is q
  - :p     probably of an empty point regrowing
  - :f     probably of lightning"
  '{:size Long, :q Number :f Number :p Number :wind sim/Wind})

(ann make-grids [plot/GnuplotP Long MultiPlotEntry MultiPlotEntry * -> (Seqable PercolationP)])
(defn make-grids [gnuplot iters & specs]
  (letfn> [start :- [MultiPlotEntry -> PercolationP]
           (start [{:keys [size q p f wind]}]
             (start! :grid (initial-grid :q q :rows size :cols size
                                         :p p :f f :wind wind)
                     :gnuplot gnuplot :no-update true))]
    (for> :- PercolationP
      [m :- MultiPlotEntry, specs]
      (-> (start m)
          (update-without-plot iters)))))

(ann do-multiplot-task1 [String -> nil])
(defn do-multiplot-task1
  "Make the plot for percolation task using several different grids"
  [prefix-str]
  (let [gp (plot/start)
        gs (make-grids
             gp
             200
             {:wind :none, :size 50,  :q 0.3, :p 0, :f 0}
             {:wind :none, :size 50,  :q 0.4, :p 0, :f 0}
             {:wind :none, :size 50,  :q 0.5, :p 0, :f 0}
             {:wind :none, :size 100, :q 0.3, :p 0, :f 0}
             {:wind :none, :size 100, :q 0.4, :p 0, :f 0}
             {:wind :none, :size 100, :q 0.5, :p 0, :f 0}
             {:wind :none, :size 200, :q 0.3, :p 0, :f 0}
             {:wind :none, :size 200, :q 0.4, :p 0, :f 0}
             {:wind :none, :size 200, :q 0.5, :p 0, :f 0})]
    (multi-plot-nburning-percolation gp gs prefix-str
                                     :report-on #{:q :size})
    #_(plot/stop gp)
    nil))

(ann do-multiplot-task2 [String -> nil])
(defn do-multiplot-task2
  "Make the plots for forest task (2) using several different grids"
  [prefix-str]
  (let [gp (plot/start)
        gs (make-grids
             gp
             100
             {:wind :none, :size 50,  :q 0.3, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 50,  :q 0.4, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 50,  :q 0.5, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 100, :q 0.3, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 100, :q 0.4, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 100, :q 0.5, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 200, :q 0.3, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 200, :q 0.4, :p 0.3, :f 6.10e-5}
             {:wind :none, :size 200, :q 0.5, :p 0.3, :f 6.10e-5})]
    (multi-plot-nburning-percolation gp gs prefix-str
                                     :report-on #{:q :size})
    #_(plot/stop gp)
    nil))

(ann do-multiplot-task3 [String -> nil])
(defn do-multiplot-task3
  "Make the plots for experimenting with changing f and p
  which keeping f/p ratio consistent."
  [prefix-str]
  (let [gp (plot/start)
        p 0.3
        f 6.10e-5
        factor1 0.5
        factor2 1
        factor3 2
        gs (make-grids
             gp
             100
             {:wind :none, :size 50,  :q 0.5, :p (* p factor1), :f (* f factor1)}
             {:wind :none, :size 50,  :q 0.5, :p (* p factor2), :f (* f factor2)}
             {:wind :none, :size 50,  :q 0.5, :p (* p factor3), :f (* f factor3)}
             {:wind :none, :size 100, :q 0.5, :p (* p factor1), :f (* f factor1)}
             {:wind :none, :size 100, :q 0.5, :p (* p factor2), :f (* f factor2)}
             {:wind :none, :size 100, :q 0.5, :p (* p factor3), :f (* f factor3)}
             {:wind :none, :size 200, :q 0.5, :p (* p factor1), :f (* f factor1)}
             {:wind :none, :size 200, :q 0.5, :p (* p factor2), :f (* f factor2)}
             {:wind :none, :size 200, :q 0.5, :p (* p factor3), :f (* f factor3)})]
    (multi-plot-nburning-percolation gp gs prefix-str
                                     :report-on #{:f :g :size})
    #_(plot/stop gp)
    nil))

(comment
(ann spit-grid [Grid IOFactory])
(defn spit-grid
  [grid path]
  (spit path (with-out-str
               (doall
               (for [row (rseq (:grid grid))]
                 (println (apply str (interpose \  (map sim/state->number row))))))
               (flush))))
  )

;; scratch below

(comment
(def proc (atom (start! :p 0 :f 0
                        :grid (initial-grid :q 0.3 :rows 200 :cols 200))))
(def proc (let [q 0.2
                p 0.3
                f 6.10e-5]
            (atom (start! :grid (initial-grid :q q :p p :f f :rows 100 :cols 100)))))
  (spit-grid (:grid @proc) "outhere")

  (dotimes [_ 10]
    (swap! proc next!))

  (output-latex-graph @proc "foobar")
  (run-percolation-test "." 0.5)
  (run-percolation-test "." 0.3)
  (run-percolation-test "." 0.4)
  (do
    (do-multiplot-task1 "task1")
    (do-multiplot-task2 "task2"))
  (do-multiplot-task3 "task3")
  )
