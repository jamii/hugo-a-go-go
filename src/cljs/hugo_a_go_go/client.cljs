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

(defn draw-circle [x y radius colour]
  (.beginPath @context)
  (set! (.-fillStyle @context) colour)
  (set! (.-strokeStyle @context) black)
  (set! (.-lineWidth @context) 1)

  (.arc @context x y radius 0 two-pi)
  (.closePath @context)
  (.fill @context)
  (.stroke @context))

(defn draw-dots []
  (doseq [y (range game-size)
          x (range game-size)]
    (draw-circle (+ (* x stone-width) stone-radius padding)
                 (+ (* y stone-width) stone-radius padding)
                 5
                 black)))

(defn draw-lines []
  (doseq [x (range game-size)]
    (let [x-start (+ (* x stone-width) stone-radius padding)
          x-end x-start
          y-start (+ stone-radius padding)
          y-end (+ (* (dec game-size) stone-width) stone-radius padding)]
      (.beginPath @context)
      (.moveTo @context x-start y-start)
      (.lineTo @context x-end y-end)
      (.stroke @context)

      (.beginPath @context)
      (.moveTo @context y-start x-start)
      (.lineTo @context y-end x-end)
      (.stroke @context)

      )))

(defn draw-pos [board y x]
  (if-let [colour (get-pos board y x)]
    (draw-circle (+ (* x stone-width) stone-radius padding)
                 (+ (* y stone-width) stone-radius padding)
                 stone-radius
                 (if (= colour :black) black white))))

(defn blank-board []
  (set! (.-fillStyle @context) background)
  (.fillRect @context 0 0 width height))

(defn display [{:keys [board] :as state}]
  (blank-board)
  (draw-lines)
  (draw-dots)
  (doseq [y (range game-size)
          x (range game-size)]
      (draw-pos board y x)))

(defn ^:export init []
  (let [board (.getElementById js/document "board")
        board-context (.getContext board "2d")
        width (.-width board)
        height (.-height board)
        ]
    (reset! context board-context)
    (display initial-state)
    ))
