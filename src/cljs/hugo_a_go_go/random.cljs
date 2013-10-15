(ns hugo-a-go-go.random
  (:require [hugo-a-go-go.board :as board]))

(defn random-int [max]
  (js/Math.floor (* max (js/Math.random))))

(defn random-move [board colour]
  (let [starting-pos (random-int board/max-pos)]
    (loop [pos starting-pos]
      (if (and (board/valid? board colour pos)
               (not (board/eyelike? board colour pos)))
        pos
        (let [new-pos (mod (inc pos) board/max-pos)]
          (if (= starting-pos new-pos)
            nil
            (recur new-pos)))))))

(defn with-random-moves [board n starting-colour]
  (loop [n n
         colour starting-colour]
    (when (> n 0)
      (when-let [move (random-move board colour)]
        (board/set-colour board move colour)
        (recur (dec n) (board/opposite-colour colour)))))
  board)

(defn random-board [n]
  (with-random-moves (board/new) n board/black))

;; massive speedup at the expense of never playing in killed spaces
(defn with-random-moves-from [board n starting-colour moves]
  (js/goog.array.shuffle moves)
  (loop [colour starting-colour]
    (if-let [move (.pop moves)]
      (board/set-colour board move colour)
      (recur (board/opposite-colour colour))))
  board)
