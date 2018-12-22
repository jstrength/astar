(ns astar.core
    (:require [reagent.core :as r]
              [cljs.core.async :as async]))

(enable-console-print!)

(def grid-size 50)
(def starting-begin-square-idx 51)
(def starting-end-square-idx (+ 48 (* 50 48)))

(defn calc-idx [x y]
  (+ x (* grid-size y)))

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
(defonce mouse-down (atom false))
(defonce running? (atom false))
(defonce step-delay (r/atom 50))

(defonce solution (r/atom "Waiting to run..."))

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
      (+ (js/Math.pow (- node-y end-y) 2)
         (js/Math.pow (- node-x end-x) 2)))))

(defn reconstruct-path [node came-from]
  (loop [path (list node)
         curr (came-from node)
         came-from (dissoc came-from node)]
    (if (came-from curr)
      (do
        (swap! (get-in @app-state [:grid curr]) assoc :type :current)
        (recur (conj path curr)
               (came-from curr)
               (dissoc came-from curr)))
      (conj path curr))))

(defn reset-grid []
  (reset! running? false)
  (reset! solution "Waiting to run...")
  (doseq [g (:grid @app-state)]
    (when-not (#{:begin :end :nowall} (:type @g))
      (swap! g assoc :type :nowall))))

(defn clear-path []
  (reset! running? false)
  (reset! solution "Waiting to run...")
  (doseq [g (:grid @app-state)]
    (when (#{:current :potential} (:type @g))
      (swap! g assoc :type :nowall))))

(defn run-astar []
  (clear-path)
  (reset! running? true)
  (reset! solution "Running...")
  (async/go-loop
    [state {:visited #{}
            :unvisited #{(:begin-square-idx @app-state)}
            :came-from {}
            :fscore {(:begin-square-idx @app-state) (heuristic-cost-estimate (:begin-square-idx @app-state))}}]

    (async/<! (async/timeout @step-delay))

    (let [closest-node (first (apply min-key second (select-keys (:fscore state) (:unvisited state))))]
      (cond
        (not @running?)
        (reset! solution "Waiting to run...")

        (empty? (:unvisited state))
        (reset! solution "No solution found.")

        (= closest-node (:end-square-idx @app-state))
        (do (println (reconstruct-path closest-node (:came-from state)))
            (reset! solution "Solution found!"))

        :else
        (recur
          (reduce
            (fn [{:keys [visited unvisited] :as new-state} neighbor]
              (when (= :nowall (:type @(get-in @app-state [:grid neighbor])))
                (swap! (get-in @app-state [:grid neighbor]) assoc :type :potential))
              (if (visited neighbor)
                new-state
                (if (unvisited neighbor)
                  new-state
                  (-> (cond-> new-state (not (unvisited neighbor)) (update :unvisited conj neighbor))
                      (update :came-from assoc neighbor closest-node)
                      (update :fscore assoc neighbor (heuristic-cost-estimate neighbor))))))
            (-> state
                (update :visited conj closest-node)
                (update :unvisited disj closest-node))
            (get-neighbors closest-node)))))))

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
                                    :current :lightgreen
                                    :potential :green
                                    :nowall :blue)}
         :on-click (fn [] (update-square-type square-data))
         :on-mouse-down (fn [] (reset! mouse-down true))
         :on-mouse-over (fn []
                          (when @mouse-down
                            (update-square-type square-data)))}])

(defn make-grid []
  (into [:div]
    (for [x (range grid-size)]
      (into
        [:div {:style {:float :left}}]
        (for [y (range grid-size)]
          [square (get-in @app-state [:grid (calc-idx x y)])])))))

(defn controls []
  [:div {:style {:margin-bottom :10px}}
   [:select {:size 4 :style {:height :100%} :value (:selection-type @app-state)
             :on-change (fn [e] (swap! app-state assoc :selection-type (keyword (.. e -target -value))))}
    [:option {:value :wall} "Draw Wall"]
    [:option {:value :nowall} "Erase Wall"]
    [:option {:value :begin} "Begining Square"]
    [:option {:value :end} "Ending Square"]]
   [:button {:style {:margin-left :10px} :on-click (fn [] (reset-grid))} "Reset Grid"]
   [:div {:style {:margin-top :10px}}
    [:button {:on-click run-astar} "Run A*"]
    [:button {:on-click clear-path :style {:margin-left :10px}} "Clear"]
    [:div {:style {:margin-top :10px}}
     [:input {:type :range :min 0 :max 100 :value @step-delay
              :step 5 :on-change #(reset! step-delay (.. % -target -value))}]
     " " @step-delay "ms"]]
   [:div @solution]])

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
