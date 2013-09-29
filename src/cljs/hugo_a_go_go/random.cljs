(ns hugo-a-go-go.random
  (:require [hugo-a-go-go.board :as board]))

(defn random-int [max]
  (js/Math.floor (* max (js/Math.random))))

(defn random-move [board colour]
  (let [starting-pos (random-int board/max-pos)]
    (loop [pos starting-pos]
      (if (and (keyword-identical? :empty (board/get-colour board pos))
               (not (board/suicide? board colour pos)))
        pos
        (let [new-pos (mod (inc pos) board/max-pos)]
          (if (= starting-pos new-pos)
            nil
            (recur new-pos)))))))

(defn random-board [n]
  (let [board (board/new)]
    (doseq [colour (take n (interleave (repeat :black) (repeat :white)))]
      (when-let [move (random-move board colour)]
        (board/set-colour board move colour)))
    board))
