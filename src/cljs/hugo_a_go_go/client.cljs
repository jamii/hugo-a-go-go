(ns hugo-a-go-go.client
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]
            [hugo-a-go-go.tree :as tree]))

(def line "#000")
(def background "ffff99")
(def white "#fff")
(def black "#000")
(def width 600)
(def height 600)
(def stone-width (/ width (+ board/size 1)))
(def stone-radius (/ stone-width 2))
(def padding stone-radius)
(def context (atom nil))

(defn log [message]
  (.log js/console message))

(defn debug-board []
  (board/debug->board [["O" "#" "#" "O" "+" "+" "O" "#" "+"]
                       ["#" "+" "O" "O" "+" "+" "O" "#" "#"]
                       ["O" "O" "O" "#" "+" "+" "O" "O" "O"]
                       ["#" "#" "O" "+" "+" "+" "+" "+" "+"]
                       ["#" "+" "O" "+" "+" "+" "+" "+" "+"]
                       ["O" "#" "#" "O" "+" "+" "+" "+" "+"]
                       ["O" "O" "O" "O" "+" "+" "+" "+" "+"]]))

(def initial-state
  {:board (debug-board)
   :to-move :black})

(defn make-move [{:keys [board to-move] :as state}
                 [x y]]
   (board/set-colour board (board/->pos x y) to-move)
  {:board board
   :to-move (if (= to-move :black) :white :black)})

(defn valid-moves [{:keys [board to-move] :as state}]
  (for [y (range board/size)
        x (range board/size)
        :when (= :empty (board/get-colour board (board/->pos x y)))
        :when (not (board/suicide? board to-move (board/->pos x y)))]
    [x y]))

;; Players are given a state and return a move
(defn random-player [state]
  (rand-nth (valid-moves state)))

(defn get-pos [board x y]
  (board/get-colour board (board/->pos x y)))

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

(defn draw-text [text x y colour]
  (set! (.-font @context) "30px Arial")
  (set! (.-fillStyle @context) colour)
  (set! (.-strokeStyle @context) colour)
  (set! (.-lineWidth @context) 1)

  (.fillText @context text x y))

(defn draw-dots []
  (doseq [y (range board/size)
          x (range board/size)]
    (draw-circle (+ (* x stone-width) stone-radius padding)
                 (+ (* y stone-width) stone-radius padding)
                 5
                 black)))

(defn draw-lines []
  (doseq [x (range board/size)]
    (let [x-start (+ (* x stone-width) stone-radius padding)
          x-end x-start
          y-start (+ stone-radius padding)
          y-end (+ (* (dec board/size) stone-width) stone-radius padding)]
      (.beginPath @context)
      (.moveTo @context x-start y-start)
      (.lineTo @context x-end y-end)
      (.stroke @context)

      (.beginPath @context)
      (.moveTo @context y-start x-start)
      (.lineTo @context y-end x-end)
      (.stroke @context))))

(defn draw-pos [board x y]
  (let [colour (get-pos board x y)
        centre-x (+ (* x stone-width) stone-radius padding)
        centre-y (+ (* y stone-width) stone-radius padding)]
    (when (#{:black :white} colour)
      (draw-circle centre-x centre-y stone-radius
                   (if (= colour :black) black white))
      (draw-text (str (board/get-liberties board (board/->pos x y)))
                 centre-x centre-y
                 (if (= colour :black) white black)))
    (when (and (= :empty colour) (board/suicide? board :white (board/->pos x y)))
      (draw-text "X" centre-x centre-y white))
    (when (and (= :empty colour) (board/suicide? board :black (board/->pos x y)))
      (draw-text "X" (- centre-x 20) (+ centre-y 20) black))))

(defn draw-score [board]
  (draw-text (board/score board) padding height black))

(defn blank-board []
  (set! (.-fillStyle @context) background)
  (.fillRect @context 0 0 width height))

(defn display [{:keys [board] :as state}]
  (blank-board)
  (draw-lines)
  (draw-dots)
  (doseq [x (range board/size)
          y (range board/size)]
    (draw-pos board x y))
  (draw-score board))

(defn ^:export init []
  (let [board (.getElementById js/document "board")
        board-context (.getContext board "2d")
        width (.-width board)
        height (.-height board)]
    (reset! context board-context)
    (display initial-state)
    #_(js/console.log (with-out-str (time (dotimes [i 1000] (random/random-board 100)))))
    (let [tree (tree/new)]
      (dotimes [_ 1]
        (tree/expand- (debug-board) tree))
      (js/console.log (tree/best-child tree)))))
