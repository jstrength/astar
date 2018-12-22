(ns astar.core
    (:require [reagent.core :as r]
              [cljs.core.async :as async]))

(enable-console-print!)

(println "This text is printed from src/astar/core.cljs. Go ahead and edit it and see reloading in action.")

(def grid-size 50)
(def starting-begin-square-idx 51)
(def starting-end-square-idx (+ 48 (* 50 48)))

(defn calc-idx [x y]
  (+ x (* grid-size y)))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (r/atom
                     {:begin-square-idx starting-begin-square-idx
                      :end-square-idx starting-end-square-idx
                      :selection-type :wall
                      :grid (vec (for [y (range grid-size) x (range grid-size)]
                                   (r/atom {:idx (calc-idx x y) :x x :y y
                                            :type (condp = (calc-idx x y)
                                                    starting-begin-square-idx :begin
                                                    starting-end-square-idx :end
                                                    :nowall)})))}))

(def mouse-down (atom false))

(defn within-bounds? [n]
  (<= 0 n (dec (* grid-size grid-size))))

(defn get-neighbors [node]
  (cond-> []
    (and (not= (mod node grid-size) (dec grid-size))
         (within-bounds? (inc node))
         (not= :wall (:type @(get-in @app-state [:grid (inc node)]))))
    (conj (inc node))

    (and (not (zero? (mod node grid-size)))
         (within-bounds? (dec node))
         (not= :wall (:type @(get-in @app-state [:grid (dec node)]))))
    (conj (dec node))

    (and (within-bounds? (+ node grid-size))
         (not= :wall (:type @(get-in @app-state [:grid (+ node grid-size)]))))
    (conj (+ node grid-size))

    (and (within-bounds? (- node grid-size))
         (not= :wall (:type @(get-in @app-state [:grid (- node grid-size)]))))
    (conj (- node grid-size))))

(defn heuristic-cost-estimate [node]
  (let [end (:end-square-idx @app-state)
        end-x (mod end grid-size)
        end-y (quot end grid-size)
        node-x (mod node grid-size)
        node-y (quot node grid-size)]
    (js/Math.sqrt
      (+ (js/Math.pow (js/Math.abs (- node-y end-y)) 2)
         (js/Math.pow (js/Math.abs (- node-x end-x)) 2)))))

(defn reconstruct-path [node came-from]
  (loop [path [node]
         curr (came-from node)
         came-from (dissoc came-from node)]
    (if (came-from curr)
      (do
        (swap! (get-in @app-state [:grid curr]) assoc :type :current)
        (recur (conj path curr)
               (came-from curr)
               (dissoc came-from curr)))
      (conj path curr))))

(def counter (atom 0))
(defn run-astar []
  (reset! counter 0)
  (loop [state {:visited #{}
                :unvisited #{(:begin-square-idx @app-state)}
                :came-from {}
                :gscore {(:begin-square-idx @app-state) 0}
                :fscore {(:begin-square-idx @app-state) (heuristic-cost-estimate (:begin-square-idx @app-state))}}]
    (swap! counter inc)
    ;(doseq [x (range 1000000)] (inc x))
    (if (> @counter 10000)
      (println "Ran out of loops")
      (do
        ;(println (count (:unvisited state)))
        ;(println (count (:visited state)))
        (if (empty? (:unvisited state))
          (println "NO SOLUTION :(")
          (let [current (first (apply min-key second (select-keys (:fscore state) (:unvisited state))))]
            ;(println (get-neighbors current))
            (if (= current (:end-square-idx @app-state))
              (do (println "FOUND END! " current)
                  (println (reconstruct-path current (:came-from state))))
              (recur
                (reduce
                  (fn [{:keys [visited unvisited gscore] :as new-state} neighbor]
                    (if (visited neighbor)
                      new-state
                      (let [tentative-gscore (+ (get gscore current 99999999) 1 ;distnace to neighbor is always 1...right?
                                                )]
                        (if (and (unvisited neighbor)
                                 (>= tentative-gscore (get gscore neighbor 99999999)))
                          new-state
                          (-> (cond-> new-state (not (unvisited neighbor)) (update :unvisited conj neighbor))
                              (update :came-from assoc neighbor current)
                              (update :gscore assoc neighbor tentative-gscore)
                              (update :fscore assoc neighbor (+ tentative-gscore (heuristic-cost-estimate neighbor))))))))
                  (-> state
                      (update :visited conj current)
                      (update :unvisited disj current))
                  (get-neighbors current))))))))))

(defn reset-grid []
  (doseq [g (:grid @app-state)]
    (when-not (#{:begin :end :nowall} (:type @g))
      (swap! g assoc :type :nowall))))

(defn clear-path []
  (doseq [g (:grid @app-state)]
    (when (= (:type @g) :current)
      (swap! g assoc :type :nowall))))

(defn update-square-type [square]
  (let [{:keys [selection-type begin-square-idx end-square-idx]} @app-state]
    (case selection-type
      :begin
      (when (not= (:idx @square) begin-square-idx)
        (swap! (get-in @app-state [:grid begin-square-idx]) assoc :type :nowall)
        (swap! square assoc :type selection-type)
        (swap! app-state assoc :begin-square-idx (:idx @square)))

      :end
      (when (not= (:idx @square) end-square-idx)
        (swap! (get-in @app-state [:grid end-square-idx]) assoc :type :nowall)
        (swap! square assoc :type selection-type)
        (swap! app-state assoc :end-square-idx (:idx @square)))

      (when-not (#{begin-square-idx end-square-idx} (:idx @square))
        (swap! square assoc :type selection-type)))))

(defn square [square-data]
  ^{:key (gensym)}
  [:div {:id (:idx @square-data)
         :style {:width :10px :height :10px :border "1px solid black"
                 :backgroundColor (case (:type @square-data)
                                    :begin :yellow
                                    :end :orange
                                    :wall :red
                                    :current :pink
                                    :nowall :blue)}
         :on-click (fn [] (update-square-type square-data))
         :on-mouse-down (fn [] (reset! mouse-down true))
         :on-mouse-over (fn []
                          (when @mouse-down
                            (update-square-type square-data)))
         }])

(defn make-grid []
  (into [:div]
    (for [x (range grid-size)]
      (into
        [:div {:style {:float :left}}]
        (for [y (range grid-size)]
          [square (get-in @app-state [:grid (calc-idx x y)])])))))

(defn controls []
  [:div {:style {:margin-bottom :10px}}
   [:select {:type :checkbox
             :on-change (fn [e] (swap! app-state assoc :selection-type (-> e .-target .-value keyword)))}
    [:option {:value :wall} "Draw Wall"]
    [:option {:value :nowall} "Erase Wall"]
    [:option {:value :begin} "Begining Square"]
    [:option {:value :end} "Ending Square"]]
   [:button {:style {:margin-left :10px} :on-click (fn [] (reset-grid))} "Reset Grid"]
   [:div {:style {:margin-top :10px}}
    [:button {:on-click (fn [] (async/go (run-astar)))} "Run A*"]
    [:button {:on-click (fn [] (clear-path)) :style {:margin-left :10px}} "Clear"]]])

(defn a* []
  (.addEventListener js/document "mouseup" (fn [] (reset! mouse-down false)))
  [:div
   [:h1 "A*"]
   [controls]
   [make-grid]])

(r/render-component [a*]
                    (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
