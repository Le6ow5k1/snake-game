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

(defn- generate-object-coordinates [board-size]
  [(rand-int (dec board-size)) (rand-int (dec board-size))])

(defn- do-generate-board-objects [board-size objects-count created-objects]
  (loop [count objects-count
         objects #{}]
    (let [object-coordinates (generate-object-coordinates board-size)
          can-place-object? (and (not (contains? objects object-coordinates))
                                 (not (contains? created-objects object-coordinates)))]
      (cond
        (= 0 count) objects
        can-place-object? (recur (dec count) (into objects [object-coordinates]))
        :else (recur count objects)))))

(defn generate-board-objects
  ([board-size objects-count]
   (do-generate-board-objects board-size objects-count #{}))
  ([board-size objects-count created-objects]
   (do-generate-board-objects board-size objects-count created-objects)))

(defn move-snake-head-to [[x y] direction]
  (case direction
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

(defn ensure-coord-within-board [coord board-size]
  (cond
    (>= coord board-size) 0
    (< coord 0) (dec board-size)
    :else coord))

(defn snake-grow-to [[head & _ :as all] direction board-size]
  (let [[x y] (move-snake-head-to head direction)
        new-x (ensure-coord-within-board x board-size)
        new-y (ensure-coord-within-board y board-size)]
    (->> all (into [[new-x new-y]]))))

(defn snake-move-to [body direction board-size]
  (pop (snake-grow-to body direction board-size)))

(defn snake-move [snake-state board-size]
  (let [direction (@snake-state :direction)]
    (swap! snake-state update :body-coordinates snake-move-to direction board-size)))

(defn meal-under-snake-head [state meal-coordinates]
  (let [[head & _] (@state :body-coordinates)]
    (some #(when (= head %) %) meal-coordinates)))

(defn snake-eat-meal [snake-state meal game-state board-size]
  (let [direction (@snake-state :direction)]
    (swap! snake-state update :body-coordinates snake-grow-to direction board-size)
    (swap! game-state update-in [:meal-coordinates] disj meal)))

(defn update-snake-state [state game-state board-size]
  (fn []
    (let [meal-coordinates (@game-state :meal-coordinates)
          meal (meal-under-snake-head state meal-coordinates)]
      (if meal
        (snake-eat-meal state meal game-state board-size)
        (snake-move state board-size)))))

(defn handle-keydown [snake-state]
  (fn [event]
    (let [key-code (.-keyCode event)
          direction (keycode->direction-map key-code)]
      (when direction
        (swap! snake-state assoc :direction direction)))))

(defn- cell-color [x y snake-coordinates meal-coordinates obstacles-coordinates]
  (cond
    (some #(= % [x y]) snake-coordinates) "black"
    (contains? meal-coordinates [x y]) "green"
    (contains? obstacles-coordinates [x y]) "red"
    :else "white"))

(defn board-component [size game-level]
  (let [snake-state (r/atom {:direction :right
                             :body-coordinates [[2 4] [2 3] [1 3] [0 3]]})
        meal-count (/ (/ size 2) game-level)
        obstacles-count (* (/ size 2) game-level)
        meal-coordinates (generate-board-objects size meal-count)
        obstacles-coordinates (generate-board-objects size obstacles-count meal-coordinates)
        game-state (r/atom {:level game-level
                            :meal-coordinates meal-coordinates
                            :obstacles-coordinates obstacles-coordinates})]
    (js/setInterval (update-snake-state snake-state game-state size) 1000)
    (js/document.addEventListener "keydown" (handle-keydown snake-state))
    (fn []
      [:div.board {:class (str "board-" size)}
       (doall (for [y (range size)
                    x (range size)
                    snake-coordinates (@snake-state :body-coordinates)
                    meal-coordinates (@game-state :meal-coordinates)
                    obstacles-coordinates (@game-state :obstacles-coordinates)
                    :let [cell-color (cell-color x y snake-coordinates meal-coordinates obstacles-coordinates)]]
                ^{:key [x y]} [cell cell-color]))])))

(defn board-20-component []
  (board-component 20 1))

(r/render [board-20-component] (js/document.getElementById "app"))
