(ns fire.simulate
  "This namespace defines operations for the study of
  percolation in the forest fire simulation."
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst
                                        letfn> override-method dotimes>]
             :as tc]
            [clojure.core.typed.hole :as h]
            (fire
              [gnuplot :as plot :refer [GnuplotP]]
              [types])
            [clojure.string :as str]
            [clojure.tools.trace :as trace])
  (:import (clojure.lang IPersistentVector IPersistentSet Seqable LazySeq)
           (java.io Writer)))

;-----------------------------------------------------------------
; Type Aliases
;-----------------------------------------------------------------

(def-alias State 
  "A point can either be empty, a tree, or a burning tree."
  (U ':burning ':tree ':empty))

(def-alias GridHistoryEntry
  "Some data about a particular state.

  - :nburning  number of burning points at this state
  - :ntrees  number of green trees at this state"
  '{:nburning AnyInteger
    :ntrees AnyInteger
    :nempty AnyInteger})

(def-alias GridHistory
  "A vector of history states on a grid"
  (IPersistentVector GridHistoryEntry))

(def-alias GridVector
  "A vector of vectors of states representing a cellular
  automata lattice."
  (IPersistentVector (IPersistentVector State)))

(def-alias Grid
  "An immutable snapshot of the world state.
  
  - :grid   the grid
  - :rows   number of rows
  - :cols   number of columns
  - :history  a chronological vector of interesting data of previous states.
              See GridHistory.
  - :q   the initial probability of a green tree at time 0 at a site
  - :p   the probability a tree will grow in an empty site
  - :f   the probability a site with a tree will burn (lightning)
  - :frame    the frame number corresponding to the grid, displayed in the gnuplot graph"
  '{:grid GridVector
    :rows AnyInteger
    :cols AnyInteger
    :history GridHistory
    :q Number
    :p Number
    :f Number
    :frame AnyInteger})

(def-alias Point 
  "A point in 2d space."
  '[AnyInteger, AnyInteger])

(def-alias Wind
  "Unused"
  Any)

;-----------------------------------------------------------------
; Utility functions
;-----------------------------------------------------------------

(ann state->number [State -> Long])
(defn state->number
  "Convert the keyword representation of a state to
  a number usable by Gnuplot for plotting color gradient."
  [k]
  (case k
    :empty 0
    :tree 1
    :burning 2))

(ann occurs? [Number -> Boolean])
(defn occurs?
  "Return true with probability p, otherwise false."
  [p]
  (< (rand) p))

(defmacro with-gunplot-toplevel
  "Run the body with gnuplot as stdout.
  Do not call with-gunplot-toplevel in the body."
  [gp & body]
  `(binding [*out* (:out ~gp)]
     ~@body
     (gnuplot-eof ~gp)))

(ann gnuplot-eof [GnuplotP -> nil])
(defn gnuplot-eof
  "Finish the gnuplot transmission and flush stdout."
  [gp]
  ; tell gnuplot we're done
  ; (an extra line to be sure)
  (println)
  (println "e")
  (flush))

;-----------------------------------------------------------------
; Grid operations
;-----------------------------------------------------------------

(ann grid-from-fn [[Point -> State] & {:rows Long, :cols Long :wind Wind} :mandatory {:q Number :p Number :f Number} -> Grid])
(defn grid-from-fn 
  "Generate a grid with dimensions rows by cols. state-fn
  is fed each Point in the grid, and should return the initial state
  at that point."
  [state-fn & {:keys [rows cols q p f] :or {rows 100 cols 100}}]
  {:pre [q p f]}
  {:grid (vec
           (for> :- (IPersistentVector State)
                 [row :- AnyInteger, (range rows)]
                 (vec
                   (for> :- State
                         [col :- AnyInteger, (range cols)]
                         (state-fn [row col])))))
   :rows rows
   :cols cols
   :history []
   :frame 0
   :q q
   :p p
   :f f})

(ann initial-grid [& {:rows Long, :cols Long} :mandatory {:q Number :p Number :f Number} -> Grid])
(defn initial-grid
  "Return the initial grid state, a vector of vectors, with each
  position initialised to :empty. If not provided, number of rows and column default
  to 100."
  [& {:keys [rows cols q p f] :or {rows 100 cols 100}}]
  {:pre [q p f]}
  (grid-from-fn (constantly :empty) :rows rows :cols cols
                :q q :p p :f f))

(ann state-at [Grid Point -> State])
(defn state-at 
  "Return the state in the provided grid, at the provided 2d point.
  Throws an exception if the point is outside the grid's dimensions."
  [{:keys [grid]} [row col]]
  (ann-form grid GridVector)
  (-> grid
      (nth row)
      (nth col)))

(ann neighbour-positions (IPersistentSet '[Long Long]))
(def neighbour-positions
  "The set of relative (row, col) coordinates to calculate
  the nearest neighbours for a point."
  #{[-1 -1] ;lower left
    [0 -1]  ;left      
    [1 -1]  ;upper left
    [1 0]   ;upper
    [1 1]   ;upper right
    [0 1]   ;right
    [-1 1]  ;lower right
    [-1 0]  ;lower
    })

(ann grid-dimensions [Grid -> '{:nrows AnyInteger, :ncols AnyInteger}])
(defn grid-dimensions [grid]
  {:nrows (:rows grid)
   :ncols (:cols grid)})

(ann neighbour-points [Grid Point -> (LazySeq Point)])
(defn neighbour-points 
  "Return a lazy sequence containing the states of neighbour points of point pnt, respecting
  periodic boundary conditions"
  [grid [^long row, ^long col, :as p]]
  (letfn> [wrap-around :- [AnyInteger AnyInteger -> AnyInteger]
           ; takes a magnitude of a dimension and the length of the
           ; dimension and corrects it for periodic boundary conditions
           (wrap-around [^long new-x ^long x-length]
             (cond
               ; wrap up
               (< new-x 0) (+ new-x x-length)
               ; wrap down
               (>= new-x x-length) (- new-x x-length)
               ; already inside
               :else new-x))]
    (let [{:keys [nrows ncols]} (grid-dimensions grid)
          ; check current point is between grid bounds
          _ (assert (<= 0 row (dec nrows)) "Row out of bounds")
          _ (assert (<= 0 col (dec ncols)) "Column out of bounds")]
      ; collect the states of each neighbour, respecting periodic boundary conditions
      (for> :- Point 
        [[^long row-diff ^long col-diff] :- '[Long Long], neighbour-positions]
        (let [neighbour-row (wrap-around (+ row row-diff) nrows)
              neighbour-col (wrap-around (+ col col-diff) ncols)]
          [neighbour-row neighbour-col])))))

(ann nearest-neighbours [Grid Point -> (LazySeq State)])
(defn nearest-neighbours 
  "Return a lazy sequence containing the states of the nearest neighbours
  of point p0 in grid. Respects periodic boundary conditions."
  [grid p0]
  (->> (neighbour-points grid p0)
       (map (fn> [p1 :- Point]
              (state-at grid p1)))))

(ann next-state [Grid State Point -> State])
(defn next-state
  "Return the state at the next time interval in grid for
  the given point, with current state s."
  [{:keys [f p] :as grid} s [row col :as point]]
  (let [neighbours (nearest-neighbours grid [row col])]
    (case s
      ;1. A burning tree becomes an empty site
      :burning :empty

      ;2. A viable tree becomes a burning tree if at least one of its nearest neighbours
      ;is burning
      ;4. A tree without a burning nearest neighbour becomes a burning tree during
      ;one time step with probability f (e.g. lightning).
      :tree 
      (if (some #(= :burning %) neighbours)
        :burning
        (if (occurs? f)
          :burning
          :tree))

      ;3. At an empty site, a tree grows with probability p
      :empty (if (occurs? p)
               :tree
               :empty))))

(ann flat-grid [Grid -> (Seqable State)])
(defn flat-grid [{:keys [grid]}]
  (ann-form grid GridVector)
  (apply concat grid))

(ann next-grid [Grid -> Grid])
(defn next-grid 
  "Simultaneously update a Grid to the next time increment
  according to the 4 update rules. Observed periodic boundary conditions.
  See next-state for the state increment."
  [grid]
  (let [; ---------------------------------------------------------------------------------
        ; START TYPE SYSTEM BOILERPLATE
        ;  we need to instantiate `vector` because core.typed's inference isn't good enough.
        ;  Semantically both row-states and col-states are exactly clojure.core/vector.
        row-states (inst vector AnyInteger (IPersistentVector State) Any Any Any Any)
        col-states (inst vector AnyInteger State Any Any Any Any)
        ; END BOILERPLATE
        ;----------------------------------------------------------------------------------
        ]
    (-> grid
      ; add to the history
      (assoc :history 
             (conj (:history grid) {:nburning (count (filter #(= :burning %) (flat-grid grid)))
                                    :ntrees   (count (filter #(= :tree %) (flat-grid grid)))
                                    :nempty   (count (filter #(= :empty %) (flat-grid grid)))}))
      (assoc :grid
             (vec
               (for> :- (IPersistentVector State)
                 [[row ss] :- '[AnyInteger (IPersistentVector State)], (map-indexed row-states (:grid grid))]
                 ; row is the row number
                 ; ss is a whole column of states
                 (vec
                   (for> :- State
                     [[col s] :- '[AnyInteger State], (map-indexed col-states ss)]
                     ; col is the col number
                     ; s is a state at a point
                     (next-state grid s [row col])))))))))


;-----------------------------------------------------------------
; gnuplot operations
;-----------------------------------------------------------------

(ann x-axis-size Long)
(def x-axis-size 500)

(ann plot-forest-via-array* [GnuplotP Grid -> nil])
(defn- plot-forest-via-array*
  "Plot the forest using raw format."
  [gp {:keys [frame] :as grid}]
  (ann-form frame Number)
  (let [{:keys [nrows ncols]} (grid-dimensions grid)]
    (println
      (str "plot '-' binary array=" nrows  "x" ncols
           " flipy format='%char' title 'Forest Fire Simulation - Frame " frame
           "' with image"))
    ; print each point to gnuplot as a char array.
    (let [^chars arr (char-array 
                       ; array length is rowsxcols
                       (* nrows ncols)
                       (map (fn> [s :- State]
                                 (-> s state->number char))
                            ; reverse the grid, we provide each row in reverse order.
                            (apply concat (rseq (:grid grid)))))]
      (.write *out* arr))
    (gnuplot-eof gp)))

(ann plot-forest [GnuplotP Grid -> nil])
(defn plot-forest
  "Plot the forest fire simulation lattice.
  Assumption: output term is already set."
  [gp {:keys [frame] :as grid}]
  (ann-form frame Number)
  ; *out* is gnuplot
  ;plot the forest
  ;#_(println "set term x11 0")
  (println (slurp "resources/setup-gnuplot.gpi"))
  (println "unset grid")
  (println "unset xlabel")
  (println "unset ylabel")
  (println "unset xtics")
  (println "unset ytics")
  (println "unset x2tics")
  (println "unset y2tics")
  (plot-forest-via-array* gp grid)
  (gnuplot-eof gp))

(ann plot-nburning-graph [GnuplotP Grid -> nil])
(defn plot-nburning-graph
  "Plot the number of burning trees over time graph. 
  Assumption: output term is already set.
  Does not flush stdout and does not end transmission to gnuplot."
  [gp {:keys [history] :as grid}]
  (ann-form history GridHistory)
  (println "set title 'Number of Burning trees'")
  (println "set xlabel 'Time'")
  (println "set ylabel 'Burning trees'")
  (println "set xtics nomirror autofreq")
  (println "set ytics nomirror autofreq")
  (println "unset x2tics")
  (println "unset y2tics")
  (println "plot '-' using 1:2 title 'Burning' with lines")
  (dotimes> [n (count history)]
    (let [{:keys [nburning]} (nth history n)]
      (ann-form nburning Number)
      (println n nburning)))
  (gnuplot-eof gp))

(ann plot-ntrees-graph [GnuplotP Grid -> nil])
(defn plot-ntrees-graph
  "Plot the number of green trees over time graph. 
  Assumption: output term is already set."
  [gp {:keys [history] :as grid}]
  (ann-form history GridHistory)
  (println "set title 'Number of Green trees'")
  (println "set xlabel 'Time'")
  (println "set ylabel 'Green trees'")
  (println "set xtics nomirror autofreq")
  (println "set ytics nomirror autofreq")
  (println "unset x2tics")
  (println "unset y2tics")
  (println "plot '-'using 1:2 title 'Green' with lines")
  (dotimes> [n (count history)]
    (let [{:keys [ntrees]} (nth history n)]
      (ann-form ntrees Number)
      (println n ntrees)))
  (gnuplot-eof gp))

(ann update-simulation! [GnuplotP Grid -> nil])
(defn update-simulation!
  "Update the simulation with the provided grid."
  [gp grid]
  ; make the forest
  (println "set term x11 0")
  (plot-forest gp grid)
  (gnuplot-eof gp)

  ; make the burning graph
  (println "set term x11 1")
  (plot-nburning-graph gp grid)
  (gnuplot-eof gp)

  ; make the green graph
  (println "set term x11 2")
  (plot-ntrees-graph gp grid)
  (gnuplot-eof gp))

(ann setup-gnuplot! [GnuplotP -> nil])
(defn setup-gnuplot! 
  "Setup the gnuplot window to prepare writing the simulation.
  Actual setup commands are in 'resources/setup-gnuplot.gpi'."
  [gp]
  (println (slurp "resources/setup-gnuplot.gpi"))
  (flush))

(comment
  (ann run-simulation! [& {:p Number :f Number} -> nil])
  (defn run-simulation!
    "Run a forest fire simulation via gnuplot."
    [& {:as opts}]
    (let [; start a new Gnuplot process
          gp (plot/start)
          ; get the initial world state
          s0 (initial-grid)]
      (setup-gnuplot! gp)
      (h/silent-hole)))
  (def current-proc (plot/start))
  (setup-gnuplot! current-proc)

  (def my-grid (atom (initial-grid)))
  (def frame-number (atom 0))
  (def my-opts {:p 0.1 :f 0.1})

  (defn my-next []
    (swap! my-grid next-grid my-opts)
    (swap! frame-number inc)
    (update-simulation! current-proc @my-grid {:time-code @frame-number}))

  (dotimes [_ 100] (my-next))

  (map identity [1 2 3]))
