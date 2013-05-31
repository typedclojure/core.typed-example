(ns fire.transient
  "This namespace is the main driver for the fire simulation."
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> dotimes>]]
            [clojure.tools.analyzer :refer [analyze-form]]
            [fire.simulate :as sim]))

(typed-deps fire.simulate)

(ann ^:nocheck create-grid [[sim/Point -> sim/State] Long Long -> sim/Grid])
(defn create-grid [state-fn ^long rows ^long cols]
  (loop [row 0
         grid-v (transient [])]
    (if (< row rows)
      (recur (inc row)
             (conj! grid-v (loop [col (long 0), col-v (transient [])]
                             (if (< col cols)
                               (recur (long (inc col)) (conj! col-v (state-fn [row col])))
                               (persistent! col-v)))))
      (persistent! grid-v))))

(ann ^:nocheck grid-from-fn [[sim/Point -> sim/State] & {:rows Long, :cols Long} -> sim/Grid])
(defn grid-from-fn 
  "Generate a grid with dimensions rows by cols. state-fn
  is fed each Point in the grid, and should return the initial state
  at that point."
  [state-fn & {:keys [rows cols] :or {rows 100 cols 100}}]
  {:grid (create-grid state-fn rows cols)
   :rows rows
   :cols cols})

(ann ^:nocheck next-grid [sim/Grid sim/GridOpt -> sim/Grid])
(defn next-grid 
  "Simultaneously update a Grid to the next time increment
  according to the 4 update rules. Observed periodic boundary conditions.
  See next-state for the state increment."
  [grid opt]
  (let [{:keys [nrows ncols]} (sim/grid-dimensions grid)]
    (-> grid
      (assoc :grid
             (create-grid (fn [[row col :as pnt]]
                            (let [s0 (sim/state-at grid pnt)]
                              (sim/next-state grid s0 pnt {:p (:p opt) :f (:f opt)})))
                          nrows ncols)))))
