(ns fire.simulate-test
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns ann typed-deps]]
            [fire.simulate :refer [Grid] :as sim]))

(defmacro deftest> [name & body]
  `(do (ann ~name ~'[-> Any])
       (deftest ~name ~@body)))

(ann ^:no-check clojure.test/test-var [clojure.lang.Var -> Any])

(deftest> check-this-file
  (is (check-ns 'fire.simulate-test)))

(ann init-grid Grid)
(def init-grid (sim/initial-grid :rows 100, :cols 100 :q 0.5 :p 0.0 :f 0.0))

(deftest> periodic-boundary-conditions-test
  (testing "Neighbours respect boundary conditions"
    (is (= (set (sim/neighbour-points init-grid [0 0]))
           #{[0 1] [1 1] [1 0] [99 1] [99 0] [99 99] [0 99] [1 99]})
          )))

(deftest> internal-type-checking
  (is (check-ns 'fire.simulate))
  (is (check-ns 'fire.main))
  (is (check-ns 'fire.gnuplot))
  (is (check-ns 'fire.simulate.percolation)))
