; start and stop heavily adapted from https://bitbucket.org/vshender/clojure-gnuplot/src/bcff38db8260059fd7114ccf202237060a472278/src/org/shender/clojure_gnuplot.clj?at=default

(ns fire.gnuplot
  (:require [clojure.core.typed :refer [ann check-ns def-alias non-nil-return print-env cf
                                        typed-deps]]
            [clojure.java.io :as io])
  (:import (java.io BufferedWriter OutputStreamWriter Writer
                    BufferedReader InputStreamReader Reader)))

(typed-deps clojure.core.typed.contrib-annotations)


;---------------------------------------
; Types
;---------------------------------------

(def-alias GnuplotP 
  "A gnuplot process.
  
  - :proc  The Process instance
  - :out   A Writer piping to gnuplot's stdin
  - :in    A Reader reading from gnuplot's stdout"
  '{:proc Process, :out Writer, :in Reader})

; We know these method never return null.
(non-nil-return java.lang.Runtime/getRuntime :all)
(non-nil-return java.lang.Process/getOutputStream :all)
(non-nil-return java.lang.Process/getInputStream :all)
(non-nil-return java.lang.Runtime/exec :all)
(non-nil-return java.lang.ProcessBuilder/start :all)

;remove when core.typed upgraded
(ann ^:nocheck clojure.java.io/writer
     [clojure.java.io/IOFactory -> java.io.BufferedWriter])


;-------------------------------------------------
; gnuplot Process ops
;-------------------------------------------------

(ann start [-> GnuplotP])
(defn start
  "Start gnuplot process."
  []
  (let [proc (-> (doto (ProcessBuilder. '("gnuplot" "-persist"))
                   (.redirectErrorStream true))
                 .start)
        out  (io/writer (.getOutputStream proc))
        in  (io/reader (.getInputStream proc))]
    {:proc proc :out out :in in}))

(ann stop [GnuplotP -> Any])
(defn stop
  "Stop gnuplot process."
  [{:keys [^Process proc]}]
  (.destroy proc))
