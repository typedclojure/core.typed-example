(ns fire.gui
  (:require [clojure.core.typed :refer [ann Option]])
  (:use [seesaw.core])
  (:import (javax.swing JFrame)))

(ann seesaw.core/native! [-> nil])
(native!)

select

(ann seesaw.core/frame
     [& {:title (Option String)}
      -> JFrame])

(ann frame
(def f (frame :title "Get to know Seesaw"))

(-> f pack! show!)

(prn (config f :title))

(config! f :title "No RLY, get to know Seesaw!")

(config! f :content "This is some content")

(def lbl (label "I'm another label"))

(config! f :content lbl)

(defn display [content]
    (config! f :content content)
    content)

(display lbl)

(config! lbl :background :pink :foreground "#00f")

(config! lbl :font "ARIAL-BOLD-21")

(use 'seesaw.font)

(config! lbl :font (font :name :monospaced 
                         :style #{:bold :italic} 
                         :size 18))

(def b (button :text "Click Me"))

(alert "I'm an alert")

(input "What's your favorite color?")
(display b)
(listen b :action (fn [e] (alert e "Thanks!")))
