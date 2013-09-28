(ns hugo-a-go-go.client)

(def line "#000")
(def background "ffff99")
(def white "#fff")
(def black "#000")
(def game-size 9)
(def width 600)
(def height 600)
(def stone-width (/ width (+ game-size 1)))
(def stone-radius (/ stone-width 2))
(def padding stone-radius)
(def context (atom nil))

(defn log [message]
  (.log js/console message))

(defn new-board [size]
  (vec (take size (repeat (vec (take size (repeat nil)))))))

(defn random-board [size]
  (vec
   (take
    size
    (repeatedly
     (fn [] (vec (take size
                       (repeatedly
                        (fn [] (rand-nth [:black :white nil]))))))))))
(def initial-state
  {:board (random-board 9)
   :to-move :black})

(defn make-move [{:keys [board to-move] :as state}
                 [y x]]
  {:board (assoc-in board [y x] to-move)
   :to-move (if (= to-move :black) :white :black)})

(defn valid-moves [{:keys [board] :as state}]
  (for [y (range (count board))
        x (range (count board))
        :when (nil? (get-in board [y x]))]
    [y x]))

;; Players are given a state and return a move
(defn random-player [state]
  (rand-nth (valid-moves state)))

(defn get-pos [board y x]
  (get-in board [y x]))

(def two-pi (* 2 (.-PI js/Math)))

(defn draw-circle [x y colour]
  (set! (.-fillStyle @context) colour)
  (.beginPath @context)
  (.arc @context x y stone-radius 0 two-pi)
  (.closePath @context)
  (.fill @context))

(defn draw-pos [board y x]
  (if-let [colour (get-pos board y x)]
    (draw-circle (+ (* x stone-width) stone-radius padding)
                 (+ (* y stone-width) stone-radius padding)
                 (if (= colour :black) black white))))

(defn blank-board []
  (set! (.-fillStyle @context) background)
  (.fillRect @context 0 0 width height))

;; (defnefn draw-pos [board [y x :as pos]]


(defn display [{:keys [board] :as state}]
  (blank-board)
  (let [size (count board)]
    (doseq [y (range size)
            x (range size)]
      )))

(defn ^:export init []
  (let [board (.getElementById js/document "board")
        board-context (.getContext board "2d")
        width (.-width board)
        height (.-height board)
        ]
    (reset! context board-context)
    (blank-board)
    ;; (draw-circle 200 100 black)
    (log (clj->js (:board initial-state)))
    (doseq [y (range game-size)
            x (range game-size)]
      (draw-pos (:board initial-state) y x)
      )))
