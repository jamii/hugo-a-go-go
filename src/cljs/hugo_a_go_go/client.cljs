(ns hugo-a-go-go.client
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]
            [hugo-a-go-go.tree :as tree])
  (:require-macros [hugo-a-go-go.macros :refer [case== get-colour get-string get-liberties get-neighbours]]))

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
(def board-ref (atom nil))
(defn log [message]
  (.log js/console (clj->js message)))

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
  {:board (board/new)
   :to-move board/black})

(defn make-move [{:keys [board to-move] :as state}
                 [x y]]
  (board/place-stone board (board/->pos x y) to-move)
  {:board board
   :to-move (if (= to-move board/black) board/white board/black)})

(defn valid-moves [{:keys [board to-move] :as state}]
  (for [y (range board/size)
        x (range board/size)
        :when (= :empty (get-colour board (board/->pos x y)))
        :when (not (board/suicide? board to-move (board/->pos x y)))]
    [x y]))

;; Players are given a state and return a move
(defn random-player [state]
  (rand-nth (valid-moves state)))

(defn get-pos [board x y]
  (get-colour board (board/->pos x y)))

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
    (when (#{board/black board/white} colour)
      (draw-circle centre-x centre-y stone-radius
                   (if (= colour board/black) black white))
      (draw-text (str (get-liberties board (board/->pos x y))
                      (char (+ 97 (get-string board (board/->pos x y)))))
                 centre-x centre-y
                 (if (= colour board/black) white black)))
    (when (and (= board/empty colour) (board/suicide? board board/white (board/->pos x y)))
      (draw-text "X" centre-x centre-y white))
    (when (and (= board/empty colour) (board/suicide? board board/black (board/->pos x y)))
      (draw-text "X" (- centre-x 20) (+ centre-y 20) black))))

(defn highlight-move [move]
  (let [x (dec (mod move board/array-size))
        y (dec (quot move board/array-size))
        centre-x (+ (* x stone-width) stone-radius padding)
        centre-y (+ (* y stone-width) stone-radius padding)]
    (outline-circle centre-x centre-y stone-radius "red")))

(defn draw-subtitle [text]
  (draw-text (str text) padding height black))

(defn blank-board []
  (set! (.-fillStyle @context) background)
  (.fillRect @context 0 0 width height))

(defn display [{:keys [board] :as state}]
  (blank-board)
  (draw-lines)
  (draw-dots)
  (doseq [x (range board/size)
          y (range board/size)]
    (draw-pos board x y)))

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
      (board/place-stone board move (.-colour tree))
      (tree/uproot child)
      (display {:board board :to-move (.-colour tree)})
      (outline-circle centre-x centre-y stone-radius "red")
      (js/window.setTimeout #(play-off board child) 0))
    (js/alert "Pass!")))

(defn play-off [board colour]
  (if-let [move (if (= board/black colour)
                  (tree/move-for board colour 1000)
                  (random/random-move board colour))]
    (let [x (dec (mod move board/array-size))
          y (dec (quot move board/array-size))
          centre-x (+ (* x stone-width) stone-radius padding)
          centre-y (+ (* y stone-width) stone-radius padding)]
      (js/console.log "move" x y move colour)
      (assert (board/valid? board colour move))
      (board/place-stone board move colour)
      (display {:board board :to-move colour})
      (outline-circle centre-x centre-y stone-radius "red")
      (js/window.setTimeout #(play-off board (board/opposite-colour colour)) 0))
    (js/alert "Pass!")))

(defn ^:export init []
  (let [board-element (.getElementById js/document "board")
        board-context (.getContext board-element "2d")]
    (reset! context board-context)
    (reset! board-ref board-element)
    (display initial-state)
    ))
