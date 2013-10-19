(ns hugo-a-go-go.tree
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]))

;; TODO value and min/max is probably wrong
;; TODO hashes for detecting repeated positions

(defrecord Node [parent board colour pos count sum nodes valids])

;; TODO should track this incrementally in the board
(defn valids [board colour]
  (let [valids (js/Uint8Array. board/max-pos)]
    (loop [pos 0
           ix 0]
      (if (< pos board/max-pos)
        (if (board/valid? board colour pos)
          (do (aset valids ix pos)
              (recur (inc pos) (inc ix)))
          (recur (inc pos) ix))
        valids))))

(defn new [board colour]
  (->Node nil board (board/opposite-colour colour) 0 0 0 (make-array 0) (valids board colour)))

(defn uproot [node]
  (set! (.-parent node) nil)
  (set! (.-pos node) 0))

(defn add-value [node ai-colour value]
  (set! (.-count node) (+ (.-count node) 1))
  (set! (.-sum node) (if (== ai-colour (.-colour node))
                       (+ (.-sum node) value)
                       (- (.-sum node) value)))
  (let [parent (.-parent node)]
    (when (not (nil? parent))
      (recur parent ai-colour value))))

(defn expand-leaf [board ai-colour parent colour pos]
  (let [board (board/copy board)]
    (board/place-stone board pos colour)
    (let [valids (valids board (board/opposite-colour colour))]
      (let [future-board (random/with-random-moves board 100 (board/opposite-colour colour))
            value (board/score future-board ai-colour)]
        (add-value parent ai-colour value)
        (->Node parent board colour pos 1 value (make-array 0) valids)))))

(def explorer-box (make-array 1))

(defn explorer [node]
  (let [nodes (.-nodes node)]
    (aset explorer-box 0 nil)
    (areduce nodes i best-score (- (/ 1 0))
             (let [child (aget nodes i)
                   score (+ (/ (.-sum child) (.-count child) (* board/size board/size))
                            (js/Math.sqrt
                             (/ (* 2 (js/Math.log (.-count node)))
                                (.-count child))))]
               (if (> score best-score)
                 (do (aset explorer-box 0 child)
                     score)
                 best-score)))
    (aget explorer-box 0)))

(defn exploiter [node]
  (let [best-score (atom (- (/ 1 0)))
        exploiter (atom nil)]
    (doseq [child (.-nodes node)]
      (let [score (/ (.-sum child) (.-count child))]
        (when (> score @best-score)
          (reset! best-score score)
          (reset! exploiter child))))
    @exploiter))

(defn pop [valids]
  (if (== 0 (aget valids 0))
    nil
    (loop [ix 1]
      (if (== 0 (aget valids ix))
        (let [result (aget valids (dec ix))]
          (aset valids (dec ix) 0)
          result)
        (recur (inc ix))))))

(defn expand [node ai-colour]
  (let [valid-pos (pop (.-valids node))]
    (if (not (nil? valid-pos))
      (.push (.-nodes node) (expand-leaf (.-board node) ai-colour node (board/opposite-colour (.-colour node)) valid-pos))
      (let [child (explorer node)]
        (if (not (nil? child))
          (expand child ai-colour)
          nil ;; no possible moves - pass
          )))))

(defn move-for [board colour n]
  (let [node (hugo-a-go-go.tree/new (board/copy board) colour)]
    (dotimes [_ n]
      (expand node colour))
    (when-let [child (exploiter node)]
      (.-pos child))))
