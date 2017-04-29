(ns snake-game.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(defn cell [id color]
  ^{:key id} [:div.board-cell {:style {:backgroundColor color}}])

(defonce snake-state (r/atom {:direction "right"
                              :body-coordinates #{[0 3] [1 3] [2 3] [2 4]}}))

(defn snake-coordinates []
  (@snake-state :body-coordinates))

(defn snake-occupied? [x y]
  ((snake-coordinates) [x y]))

(defn board-component [size]
  [:div.board {:class (str "board-" size)}
   (for [y (range size)
         x (range size)
         :let [id (str x y)
               snake-cell? (snake-occupied? x y)
               cell-color (if snake-cell? "black" "white")]]
     [cell id cell-color])])

(defn board-20-component []
  (board-component 20))

(r/render [board-20-component] (js/document.getElementById "app"))
