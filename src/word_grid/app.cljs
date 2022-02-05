(ns word-grid.app
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.parametric :as p]
            [svg-clj.layout :as lo]
            [svg-clj.composites :as comp :refer [svg]]
            [goog.events :as events])
  (:import [goog.events EventType]))

;; Constants and references

(def tile-w 40)
(def tile-pad 4)
;; grid-unit
(def gu (+ tile-w tile-pad))

(def window (r/atom [js/window.innerWidth js/window.innerHeight]))
(defn ww [] (first  @window))
(defn wh [] (second @window))

;; one tile -> {:pos [0 0] :letter "A"}
(defn rect-grid
  [nx ny x-spacing y-spacing]
  (for [b (range ny)
        a (range nx)]
    [(* a x-spacing) (* b y-spacing)]))

(def tiles
  (let [letters (shuffle
                 (concat (repeat 13 "A")
                         (repeat  3 "B")
                         (repeat  3 "C")
                         (repeat  6 "D")
                         (repeat 18 "E")
                         (repeat  3 "F")
                         (repeat  4 "G")
                         (repeat  3 "H")
                         (repeat 12 "I")
                         (repeat  2 "J")
                         (repeat  2 "K")
                         (repeat  5 "L")
                         (repeat  3 "M")
                         (repeat  8 "N")
                         (repeat 11 "O")
                         (repeat  3 "P")
                         (repeat  2 "Q")
                         (repeat  9 "R")
                         (repeat  6 "S")
                         (repeat  9 "T")
                         (repeat  6 "U")
                         (repeat  3 "V")
                         (repeat  3 "W")
                         (repeat  2 "X")
                         (repeat  3 "Y")
                         (repeat  2 "Z")))
        f (fn [pos letter] {:pos pos :letter letter :visible? false})
        wwh (Math/floor (/ (ww) 2))
        whh (Math/floor (/ (wh) 2))
        grid (->> (rect-grid 7 (Math/ceil (/ (count letters) 7)) gu gu)
                  #_(partition 2)
                  #_(map first)
                  (map #(utils/v+ % [(/ gu 2) (/ gu 2)]))
                  (map #(utils/v+ % [(* gu -3) (* gu -1)]))
                  (map #(utils/v+ % [(- wwh (mod wwh gu))
                                     (- whh (mod whh gu))])))
        tiles (map f grid letters)]
    (r/atom (zipmap (range) tiles))))

(def board-pos (r/atom [(/ gu 2) (/ gu 2)]))

;; https://stackoverflow.com/a/25123243
(defn split-when [f s]
  (reduce (fn [acc [a b]]
            (if (f b a)
              (conj acc [b])
              (update-in acc [(dec (count acc))] conj b)))
          [[(first s)]]
          (partition 2 1 s)))

(defn- overlaps?
  [tiles]
  (->> tiles
       vals
       (filter :visible?)
       (map :pos)
       frequencies
       vals
       (some #(= 2 %))))

(defn- get-ids-by-pos
  [pos tiles]
  (let [ts (map (fn [[id tile]] (assoc tile :id id)) tiles)]
    (->> ts
         (filter #(= pos (:pos %)))
         (map :id))))

(defn- move-tile-down
  [id tiles]
  (update-in tiles [id :pos] #(utils/v+ % [0 gu])))

(defn fix-overlaps
  [tiles]
  (if (not (overlaps? tiles))
    tiles
    (let [positions (map (fn [[_ {:keys [pos]}]] pos) tiles)
          overlap-ids (filter #(> (count %) 1) (map #(get-ids-by-pos % tiles) positions))]
      (recur (move-tile-down (first (first overlap-ids)) tiles)))))

(defn fix-overlaps! []
  (let [fixed (fix-overlaps @tiles)]
    (reset! tiles fixed)))

(defn visible?
  [id]
  (get-in @tiles [id :visible?]))

(defn snap-tiles! []
  (let [snap-tile
        (fn [[x y]] [(Math/floor (- (+ x (/ gu 2.0)) (mod x gu)))
                     (Math/floor (- (+ y (/ gu 2.0)) (mod y gu)))])]
    (doseq [id (filter visible? (range 145))]
      (swap! tiles update-in [id :pos] snap-tile))))

(defn update-state! []
  (snap-tiles!)
  (fix-overlaps!))

(defn get-words []
  (let [f (fn [[_ {:keys [pos letter visible?]}]]
            (when visible?
              {:x (int (/ (first pos) gu))
               :y (int (/ (second pos) gu))
               :letter letter}))
        rows (->> (map f @tiles)
                  (remove nil?)
                  (sort-by :y)
                  (partition-by #(get % :y))
                  (map (fn [s] (sort-by :x s)))
                  (mapcat #(split-when (fn [a b] (not= (:x a) (inc (:x b)))) %))
                  (map (fn [s] (apply str (map :letter s))))
                  (remove #(= 1 (count %))))
        cols (->> (map f @tiles)
                  (remove nil?)
                  (sort-by :x)
                  (partition-by #(get % :x))
                  (map (fn [s] (sort-by :y s)))
                  (mapcat #(split-when (fn [a b] (not= (:y a) (inc (:y b)))) %))
                  (map (fn [s] (apply str (map :letter s))))
                  (remove #(= 1 (count %))))]
    {:horizontal rows
     :vertical cols}))

;; Utility functions
(defn get-client-mid [evt]
  (let [r (.getBoundingClientRect (.-target evt))
        left (.-left r)
        top (.-top r)
        bottom (.-bottom r)
        right (.-right r)
        w (- right left)
        h (- bottom top)]
    {:x (- right (/ w 2.0))
     :y (- bottom (/ h 2.0))}))

(defn prevent-motion [e]
    (js/window.scrollTo 0 0)
    (e.preventDefault)
    (e.stopPropagation))

(events/listen js/window EventType.SCROLL prevent-motion)
(events/listen js/window EventType.TOUCHMOVE prevent-motion)

;; Event handlers
(defn mouse-move-handler [id offset]
  (fn [evt]
    (let [cx (.-clientX evt)
          cy (.-clientY evt)
          ox (:x offset)
          oy (:y offset)]
      (if (= id :board)
        (reset! board-pos [(Math/floor (- cx ox))
                           (Math/floor (- cy oy))])
        (swap! tiles assoc-in [id :pos] [(Math/floor (- cx ox))
                                         (Math/floor (- cy oy))])))))

(defn mouse-up-handler [on-move]
  (fn me [evt]
    (events/unlisten js/window EventType.MOUSEMOVE
                     on-move)))

(defn mouse-down-handler [id]
  (fn [e]
    (let [{:keys [x y] :as mid} (get-client-mid e)
          [bx by] (utils/v* [-1 -1] @board-pos)
          cx (.-clientX e)
          cy (.-clientY e)
          offset (if (= id :board)
                   {:x (+ cx bx) :y (+ cy by)}
                   {:x (- cx (when-not (= id :board) x) bx)
                    :y (- cy (when-not (= id :board) y) by)})
          on-move (mouse-move-handler id offset)]
      (events/listen js/window EventType.MOUSEMOVE
                     on-move)
      (events/listen js/window EventType.MOUSEUP
                     (mouse-up-handler on-move)))))

(defn touch-move-handler [id offset]
  (fn [evt]
    (let [cx (.-clientX evt)
          cy (.-clientY evt)
          ox (:x offset)
          oy (:y offset)]
      (if (= id :board)
        (reset! board-pos [(Math/floor (- cx ox))
                           (Math/floor (- cy oy))])
        (swap! tiles assoc-in [id :pos] [(Math/floor (- cx ox))
                                         (Math/floor (- cy oy))])))))

(defn touch-end-handler [on-move]
  (fn te [evt]
    (events/unlisten js/window EventType.TOUCHMOVE
                     on-move)))

(defn touch-start-handler [id]
  (fn [e]
    (let [{:keys [x y] :as mid} (get-client-mid e)
          [bx by] (utils/v* [-1 -1] @board-pos)
          cx (.-pageX (first (.-changedTouches e)))
          cy (.-pageY (first (.-changedTouches e)))
          offset (if (= id :board)
                   {:x (+ cx bx) :y (+ cy by)}
                   {:x (- cx (when-not (= id :board) x) bx)
                    :y (- cy (when-not (= id :board) y) by)})
          on-move (touch-move-handler id offset)]
      (e.preventDefault)
      (events/listen js/window EventType.TOUCHMOVE
                     on-move)
      (events/listen js/window EventType.TOUCHEND
                     (touch-end-handler on-move)))))

;; components
(defn tile [[id {:keys [pos letter visible?]}]]
  (when visible?
    (-> (el/g
         #_(el/g (for [i (reverse (range 0 4))]
                 (-> (el/rect tile-w tile-w)
                     (tf/translate [(* i 0.8) i])
                     (tf/style {:rx 5
                                :fill "#EBE89A"
                                :stroke "#9E9C59"
                                :stroke-width 1.5}))))
         (-> (el/rect tile-w tile-w)
             (tf/style {:rx 5
                        :fill "#EBE89A"
                        :stroke "#9E9C59"
                        :stroke-width 1
                        #_#_:filter "drop-shadow(1px 1px 4px rgb(0 0 0 / 0.2))"}))
         #_(el/text (str pos))
         (-> (el/text letter)
             (tf/translate [0 4])
             (tf/style {:fill "#9E6067"
                        :font-size (str (* 0.625 gu) "px")
                        :font-family "Palatino"
                        :style {:user-select "none"
                                :-moz-user-select "none"
                                :-webkit-user-select "none"}})))
        
        (tf/translate (utils/v+ pos @board-pos))
        (tf/style {:on-mouse-down (mouse-down-handler id)
                   :on-touch-start (touch-start-handler id)}))))

(defn wordlist []
  (let [scrabble-finder-url "https://scrabble.merriam.com/finder/"
        {:keys [horizontal vertical]} (get-words)
        lh 20
        vlist-offset (* (+ 3 (count horizontal)) lh)
        f (fn [pos]
            (fn [idx t]
              [:a {:key idx
                   :xlinkHref (str scrabble-finder-url t)
                   :target "_blank"}
               (-> (el/text t)
                   (tf/translate pos)
                   (tf/translate [0 (* idx lh)])
                   (tf/style {:fill "#9E6067"
                              :text-decoration "underline"}))]))]
    (el/g
     (-> (el/text "HORIZONTAL:")
         (tf/translate [50 lh]))
     (map-indexed (f [50 (* 2 lh)]) horizontal)
     (-> (el/text "VERTICAL:")
         (tf/translate [50 vlist-offset]))
     (map-indexed (f [50 (+ lh vlist-offset)]) vertical))))

(defn center-button-pos []
  [(* 1 gu)
   (- (wh) (* 0.625 gu))])

(defn center! [e]
  (reset! board-pos [0 0]))

(defn center-button []
  (-> (el/g
       (-> (el/rect (* 2 tile-w) tile-w)
           (tf/style {:rx 5
                      :stroke "#596B4C"
                      :stroke-width 1
                      :fill "#C3EBA7"
                      :filter "drop-shadow(1px 1px 4px rgb(0 0 0 / 0.2))"}))
       (-> (el/text "CENTER")
           (tf/translate [0 3])
           (tf/style {:fill "#596B4C"
                      :opacity 1
                      :font-size (str (* 0.375 gu) "px") 
                      :font-family "Palatino"
                      :style {:user-select "none"
                              :-moz-user-select "none"
                              :-webkit-user-select "none"}})))
      (tf/translate (center-button-pos))
      (tf/style {:on-click center!
                 #_#_:on-touch-start center!})))

(def dump-target-line (r/atom nil))
(defn dump-button-pos []
  [(- (ww) (* 1 gu))
   (- (wh) (* 0.625 gu))])
(defn show-dump-target [e]
  (let [positions (->> @tiles
                       (filter (fn [[_ {:keys [visible?]}]] visible?))
                       (map (fn [[_ {:keys [pos]}]] pos)))
        pta (->> (sort-by #(utils/distance (utils/v+ % @board-pos) (dump-button-pos)) positions)
                first
                (utils/v+ @board-pos))
        ptc (dump-button-pos)
        ptb (utils/v+ ((p/line pta ptc) 0.5) ((p/circle (* 4 gu)) (rand)))]
    (reset! dump-target-line
            (-> (path/bezier pta ptb ptc)
                (tf/style {:stroke-linecap "round"
                           :fill "none"
                           :stroke-width 7
                           :stroke "#EB838E"
                           :opacity 0.3})))
    (js/setTimeout #(reset! dump-target-line nil) 700)))

(defn dump! [e]
  (let [ts @tiles
        positions (->> ts
                       (filter (fn [[_ {:keys [visible?]}]] visible?))
                       (map (fn [[id {:keys [pos]}]] [id pos])))
        to-dump (->> positions
                     (sort-by #(utils/distance (utils/v+ (second %) @board-pos) (dump-button-pos)))
                     first)
        to-show (->> ts
                     (remove (fn [[_ {:keys [visible?]}]] visible?))
                     (map (fn [[id {:keys [pos]}]] id))
                     (take 3))]
    (doseq [to-show (map-indexed vector (sort to-show))]
      (let [position [(- (/ (ww) 2) (mod (/ (ww) 2) gu) (- gu) (* (first to-show) gu))
                      (- (wh) (mod (wh) gu) (* 2 gu))]]
        (swap! tiles assoc-in [(second to-show) :pos] (utils/v- position @board-pos))
        (swap! tiles assoc-in [(second to-show) :visible?] true)))
    (swap! tiles assoc-in [(first to-dump) :visible?] false)))

(defn dump-button []
  (-> (el/g
       (-> (el/rect (* 2 tile-w) tile-w)
           (tf/style {:rx 5
                      :stroke "#9E6067"
                      :stroke-width 1
                      :fill "#EB838E"
                      :filter "drop-shadow(1px 1px 4px rgb(0 0 0 / 0.2))"}))
       (-> (el/text "DUMP")
           (tf/translate [0 3])
           (tf/style {:fill "#9E6067"
                      :opacity 1
                      :font-size (str (* 0.375 gu) "px") 
                      :font-family "Palatino"
                      :style {:user-select "none"
                              :-moz-user-select "none"
                              :-webkit-user-select "none"}})))
      (tf/translate (dump-button-pos))
      (tf/style {:on-click dump!
                 :on-touch-end dump!
                 :on-mouse-over show-dump-target
                 :on-touch-start show-dump-target})))

(defn add-button-pos []
  [(/ (ww) 2)
   (- (wh) (* 0.625 gu))])

(defn add! []
  (let [ts @tiles
        positions (->> ts
                       (filter (fn [[_ {:keys [visible?]}]] visible?))
                       (map (fn [[id {:keys [pos]}]] [id pos])))
        to-show (->> ts
                     (remove (fn [[_ {:keys [visible?]}]] visible?))
                     (map (fn [[id {:keys [pos]}]] id))
                     (take 1))]
    (doseq [to-show (map-indexed vector (sort to-show))]
      (let [position [(- (/ (ww) 2) (mod (/ (ww) 2) gu) (- gu) (* (first to-show) gu))
                      (- (wh) (mod (wh) gu) (* 2 gu))]]
        (swap! tiles assoc-in [(second to-show) :pos] (utils/v- position @board-pos))
        (swap! tiles assoc-in [(second to-show) :visible?] true)))))

(defn add-button []
  (-> (el/g
       (-> (el/rect (* 2 tile-w) tile-w)
           (tf/style {:rx 5
                      :stroke "#596B4C"
                      :stroke-width 1
                      :fill "#C3EBA7"
                      :filter "drop-shadow(1px 1px 4px rgb(0 0 0 / 0.2))"}))
       (-> (el/text "ADD")
           (tf/translate [0 3])
           (tf/style {:fill "#596B4C"
                      :opacity 1
                      :font-size (str (* 0.375 gu) "px") 
                      :font-family "Palatino"
                      :style {:user-select "none"
                              :-moz-user-select "none"
                              :-webkit-user-select "none"}})))
      (tf/translate (add-button-pos))
      (tf/style {:on-click add!
                 #_#_:on-touch-end add!})))

(defn share-button-pos []
  [(- (ww) (* 1 gu))
   (* 0.625 gu)])

(defn share! []
  (let [text "ON CLIPBOARD"]
    (js/navigator.clipboard.writeText text)))

(defn share-button []
  (-> (el/g
       (-> (el/rect (* 2 tile-w) tile-w)
           (tf/style {:rx 5
                      :stroke "#596B4C"
                      :stroke-width 1
                      :fill "#C3EBA7"
                      :filter "drop-shadow(1px 1px 4px rgb(0 0 0 / 0.2))"}))
       (-> (el/text "SHARE")
           (tf/translate [0 3])
           (tf/style {:fill "#596B4C"
                      :opacity 1
                      :font-size (str (* 0.375 gu) "px") 
                      :font-family "Palatino"
                      :style {:user-select "none"
                              :-moz-user-select "none"
                              :-webkit-user-select "none"}})))
      (tf/translate (share-button-pos))
      (tf/style {:on-click share!
                 #_#_:on-touch-end add!})))

(def titles
  ["Wow, it's words!"
   "Oops, all letters!"
   "You've seen a crossword before, right?"
   "WWWWWWOOOOORRRDDDDS!"
   "I like word games, ok?"
   "Naming things is hard. This is a game with letters."
   "grid + letters = words + fun"
   "Clean up these tiles, please."
   "Grams. Grams. Grams."
   "Letters make words. Words are fun."
   "Do not tolerate intolerance."
   "My name is Adam and I've made this game for you (and me)."
   "Please be kind to other people."])
(def rand-title (first (shuffle titles)))

(defn title []
  (-> (el/text rand-title)
      (tf/translate [(/ (ww) 2) (* (wh) 0.4)])
      (tf/style {:fill "#9E6067"
                 :opacity 0.1
                 :font-size "40px"
                 :font-family "Palatino"
                 :style {:user-select "none"
                         :-moz-user-select "none"
                         :-webkit-user-select "none"}})))

(defn board []
  (let [table (-> (el/rect (* 4 (ww)) (* 4 (wh)))
                  (tf/translate [(* 2 (ww)) (* 2 (wh))])
                  (tf/style {:fill "#B2D8EB"
                             :on-mouse-down (mouse-down-handler :board)
                             :on-touch-start (touch-start-handler :board)}))]
    (-> (el/g
         table
         [title]

         ;; debug
         #_(-> (el/text (str @window))
             (tf/translate [(/ (ww) 2) (/ (wh) 2)]))

         [wordlist]
         (el/g (map tile @tiles))
         @dump-target-line
         [center-button]
         [add-button]
         [dump-button]
         #_[share-button])
        (svg (ww) (wh))
        (tf/style {:style {:position "absolute" :left 0 :top 0}}))))

(defn app []
  [:<>
   [board]
   [:span (str @board-pos)]])

(defn mount [app]
  (doseq [id (range 21)] (swap! tiles assoc-in [id :visible?] true))
  (events/listen js/window EventType.MOUSEUP update-state!)
  (events/listen js/window EventType.TOUCHEND update-state!)
  (events/listen js/window EventType.RESIZE #(reset! window [js/window.innerWidth js/window.innerHeight]))
  (r/render-component [app] (js/document.getElementById "root")))

(defn init [] (mount app))
(defn ^:dev/after-load re-render [] (mount app))
