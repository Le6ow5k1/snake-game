(ns snake-game.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(def keycode->direction-map {37 :left
                             38 :up
                             39 :right
                             40 :down})

(defn cell [color]
  [:div.board-cell {:style {:backgroundColor color}}])

(defn snake-occupied? [snake-state x y]
  (some #(= % [x y]) (@snake-state :body-coordinates)))

(defn move-snake-head-to [[x y] direction]
  (case direction
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

(defn snake-move-to [[head & _ :as all] direction]
  (let [new-head (move-snake-head-to head direction)]
    (->> all (into [new-head]) pop)))

(defn snake-move [snake-state]
  (let [direction (@snake-state :direction)]
    (swap! snake-state update :body-coordinates snake-move-to direction)))

(defn update-snake-state [state]
  (fn []
    (snake-move state)))

(defn handle-keydown [snake-state]
  (fn [event]
    (let [key-code (.-keyCode event)
          direction (keycode->direction-map key-code)]
      (when direction
        (swap! snake-state assoc :direction direction)))))

(defn board-component [size]
  (let [snake-state (r/atom {:direction :right
                             :body-coordinates [[2 4] [2 3] [1 3] [0 3]]})]
    (js/setInterval (update-snake-state snake-state) 300)
    (js/document.addEventListener "keydown" (handle-keydown snake-state))
    (fn []
      [:div.board {:class (str "board-" size)}
       (doall (for [y (range size)
                    x (range size)
                    :let [snake-cell? (snake-occupied? snake-state x y)
                          cell-color (if snake-cell? "black" "white")]]
                ^{:key [x y]} [cell cell-color]))])))

(defn board-20-component []
  (board-component 20))

(r/render [board-20-component] (js/document.getElementById "app"))
