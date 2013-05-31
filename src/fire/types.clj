(ns fire.types
  (:require [clojure.core.typed :refer [ann-protocol ann]])
  (:import (clojure.lang Seqable)))

(ann-protocol clojure.java.io/IOFactory 
              make-reader
              [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedReader]
              make-writer 
              [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedWriter]
              make-input-stream 
              [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedInputStream]
              make-output-stream
              [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedOutputStream])

(ann ^:nocheck clojure.core/*in* java.io.Reader)
(ann ^:nocheck clojure.core/line-seq [java.io.BufferedReader -> (Seqable String)])

(ann ^:nocheck clojure.core/slurp [clojure.java.io/IOFactory -> String])

(ann ^:nocheck clojure.java.io/reader
     [clojure.java.io/IOFactory -> java.io.BufferedReader])
(ann ^:nocheck clojure.java.io/writer
     [clojure.java.io/IOFactory -> java.io.BufferedWriter])
(ann ^:nocheck clojure.java.io/input-stream
     [clojure.java.io/IOFactory -> java.io.BufferedInputStream])
(ann ^:nocheck clojure.java.io/output-stream
     [clojure.java.io/IOFactory -> java.io.BufferedOutputStream])
