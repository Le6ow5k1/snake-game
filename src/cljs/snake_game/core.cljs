(ns snake-game.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(def board-size 40)
(def game-level 2)

(def keycode->direction-map {37 :left
                             38 :up
                             39 :right
                             40 :down})

(def game-state
  (r/atom {:game-level nil
           :meal-coordinates nil
           :obstacles-coordinates nil
           :snake-direction nil
           :snake-coordinates nil}))

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

(defn snake-move []
  (let [{:keys [board-size snake-direction]} @game-state]
    (swap! game-state update :snake-coordinates snake-move-to snake-direction board-size)))

(defn object-under-snake-head [snake-coordinates objects-coordinates]
  (let [[head & _] snake-coordinates]
    (some #(when (= head %) %) objects-coordinates)))

(defn snake-eat-meal [meal]
  (let [{:keys [board-size snake-direction]} @game-state]
    (swap! game-state update :snake-coordinates snake-grow-to snake-direction board-size)
    (swap! game-state update-in [:meal-coordinates] disj meal)))

(declare rerender)

(defn update-game-state []
  (let [{:keys [snake-coordinates meal-coordinates obstacles-coordinates]} @game-state
        meal (object-under-snake-head snake-coordinates meal-coordinates)
        obstacle (object-under-snake-head snake-coordinates obstacles-coordinates)
        snake-collide? (object-under-snake-head snake-coordinates (rest snake-coordinates))]
    (cond
      meal (snake-eat-meal meal)
      (or obstacle snake-collide?) (do (js/alert "You've lost :(")
                                       (rerender game-level board-size))
      :else (snake-move))))

(defn handle-keydown [event]
  (let [key-code (.-keyCode event)
        direction (keycode->direction-map key-code)]
    (when direction
      (swap! game-state assoc :snake-direction direction))))

(defn init-game-state! [{:keys [board-size game-level] :as given-state}]
  (let [objects-count (* (/ board-size 2) game-level)
        meal-coordinates (generate-board-objects board-size objects-count)
        obstacles-coordinates (generate-board-objects board-size objects-count meal-coordinates)
        defaults (merge given-state {:meal-coordinates meal-coordinates
                                     :obstacles-coordinates obstacles-coordinates})]
    (swap! game-state merge defaults)))

(defn init-game! [board-size game-level]
  (let [update-game-state-interval-new (js/setInterval update-game-state (/ 500 (* game-level 0.6)))
        keydown-listener-new (js/document.addEventListener "keydown" handle-keydown)
        {:keys [update-game-state-interval keydown-listener]} @game-state]
    (when update-game-state-interval (js/clearInterval update-game-state-interval))
    (when keydown-listener (js/document.removeEventListener keydown-listener))
    (init-game-state! {:board-size board-size
                       :game-level game-level
                       :update-game-state-interval update-game-state-interval-new
                       :keydown-listener keydown-listener-new
                       :snake-direction :right
                       :snake-coordinates [[2 4] [2 3] [1 3] [0 3]]})))

(defn cell [type]
  [:div.board-cell {:class type}])

(defn- type-of-cell [x y]
  (let [{:keys [snake-coordinates meal-coordinates obstacles-coordinates]} @game-state]
    (cond
      (some #(= % [x y]) snake-coordinates) :snake
      (contains? meal-coordinates [x y]) :meal
      (contains? obstacles-coordinates [x y]) :obstacle
      :else :free-space)))

(defn summary-component []
  (let [{:keys [game-level snake-coordinates meal-coordinates]} @game-state]
    [:div.summary
     [:p (str "Level: " game-level)]
     [:p (str "Snake size: " (count snake-coordinates))]
     [:p (str "Meal: " (count meal-coordinates))]]))

(defn board-component [size]
  [:div
   [:div.board {:class (str "board-" size)}
    (doall (for [y (range size)
                 x (range size)]
             ^{:key [x y]} [cell (type-of-cell x y)]))]])

(defn game-component [game-level board-size]
  (init-game! board-size game-level)
  (fn []
    [:div
     [summary-component]
     [board-component board-size]]))

(defn render [game-level board-size]
  (r/render
   [game-component game-level board-size]
   (js/document.getElementById "app")))

(defn rerender [size]
  (r/unmount-component-at-node (js/document.getElementById "app"))
  (render game-level board-size))

(render game-level board-size)
