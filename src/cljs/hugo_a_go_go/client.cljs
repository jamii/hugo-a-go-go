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

(defn easy-board []
  (board/debug->board [["#" "#" "#" "#" "#" "#" "#" "#" "#"]
                       ["#" "#" "#" "#" "#" "#" "#" "#" "#"]
                       ["#" "#" "+" "#" "+" "#" "#" "#" "#"]
                       ["#" "#" "#" "#" "#" "#" "#" "#" "#"]
                       ["#" "#" "#" "#" "#" "#" "#" "#" "#"]
                       ["O" "O" "O" "O" "O" "O" "O" "O" "O"]
                       ["O" "O" "O" "O" "O" "O" "O" "O" "O"]
                       ["O" "O" "O" "O" "+" "O" "O" "O" "O"]
                       ["O" "O" "O" "O" "O" "O" "O" "O" "O"]]))

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

(defn outline-circle [x y radius colour]
  (.beginPath @context)
  (set! (.-strokeStyle @context) colour)
  (set! (.-lineWidth @context) 3)

  (.arc @context x y radius 0 two-pi)
  (.closePath @context)
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
  (set! (.-strokeStyle @context) black)
  (set! (.-lineWidth @context) 1)
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

#_(defn play-off [board tree]
  (dotimes [_ 1000]
    (tree/expand (board/copy board) tree))
  (if-let [child (tree/best-child tree)]
    (let [move (.-pos child)
          x (dec (mod move board/array-size))
          y (dec (quot move board/array-size))
          centre-x (+ (* x stone-width) stone-radius padding)
          centre-y (+ (* y stone-width) stone-radius padding)]
      (js/console.log "move" x y move (.-colour child))
      (assert (board/valid? board (.-colour child) move))
      (board/set-colour board move (.-colour tree))
      (tree/uproot child)
      (display {:board board :to-move (.-colour tree)})
      (outline-circle centre-x centre-y stone-radius "red")
      (js/window.setTimeout #(play-off board child) 0))
    (js/alert "Pass!")))

(defn play-off [board colour]
  (if-let [move (if (= :black colour)
                  (tree/move-for board colour 1000)
                  (random/random-move board colour))]
    (let [x (dec (mod move board/array-size))
          y (dec (quot move board/array-size))
          centre-x (+ (* x stone-width) stone-radius padding)
          centre-y (+ (* y stone-width) stone-radius padding)]
      (js/console.log "move" x y move colour)
      (assert (board/valid? board colour move))
      (board/set-colour board move colour)
      (display {:board board :to-move colour})
      (outline-circle centre-x centre-y stone-radius "red")
      (js/window.setTimeout #(play-off board (board/opposite-colour colour)) 0))
    (js/alert "Pass!")))

(defn ^:export init []
  (let [board (.getElementById js/document "board")
        board-context (.getContext board "2d")]
    (reset! context board-context)
    (js/console.log "black is ai, white is random")
    ;; (play-off (board/new) :black)
    (play-off (easy-board) :black)
    ;; (hugo-a-go-go.play.play-random-game)
    ))
