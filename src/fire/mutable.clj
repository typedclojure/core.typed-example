;this namespace isn't used yet, but useful as a test for core.typed's
;array support
(ns fire.mutable
  (:require [clojure.core.typed :refer [ann check-ns tc-ignore dotimes> def-alias typed-deps
                                        AnyInteger]]
            [fire.simulate :as sim]))

(typed-deps fire.simulate)

(def-alias MutableGrid
  "A mutable representation of the world state.
  
  - :grid   the grid
  - :rows   number of rows
  - :cols   number of columns"
  '{:grid (Array (Array sim/State))
    :rows AnyInteger,
    :cols AnyInteger})

(tc-ignore
(defn- array? [x] (-> x class .isArray))
(defn- see [x] (if (array? x) (map see x) x))
  )

(ann grid-from-fn [[sim/Point -> sim/State] & {:rows Long, :cols Long} -> MutableGrid])
(defn grid-from-fn 
  "Generate a grid with dimensions rows by cols. state-fn
  is fed each Point in the grid, and should return the initial state
  at that point."
  [state-fn & {:keys [rows cols] :or {rows 100 cols 100}}]
  {:grid 
   (let [^objects arr (make-array Object rows cols)]
     (dotimes> [row rows]
       (dotimes> [col cols]
         (let [^objects col-arr (aget arr row)]
           (prn (aset col-arr col (state-fn [row col]))))))
     arr)
   :rows rows
   :cols cols})

(ann state-at [MutableGrid Point -> State])
(defn state-at 
  "Return the state in the provided grid, at the provided 2D point.
  Throws an exception if the point is outside the grid's dimensions."
  [grid [row col]]
  (let [^objects arr (:grid grid)
        ^objects col-arr (aget arr row)]
    (aget col-arr col)))
