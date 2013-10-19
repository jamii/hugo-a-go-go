(ns hugo-a-go-go.tree
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]))

;; TODO value and min/max is probably wrong
;; TODO hashes for detecting repeated positions

(defrecord Node [parent board colour pos count sum nodes])

(defn next-valid [board colour last-valid]
  (loop [pos (inc last-valid)]
    (if (< pos board/max-pos)
      (if (board/valid? board colour pos)
        pos
        (recur (inc pos)))
      pos)))

(defn new [board colour]
  (->Node nil board (board/opposite-colour colour) 0 0 0 nil))

(defn uproot [node]
  (set! (.-parent node) nil)
  (set! (.-pos node) 0))

(defn add-value [node ai-colour sum count]
  (set! (.-count node) (+ (.-count node) count))
  (set! (.-sum node) (if (== ai-colour (.-colour node))
                       (+ (.-sum node) sum)
                       (- (.-sum node) sum)))
  (let [parent (.-parent node)]
    (when (not (nil? parent))
      (recur parent ai-colour sum count))))

(defn leaf [board ai-colour parent colour pos]
  (let [board (board/copy board)]
    (board/place-stone board pos colour)
    (let [future-board (random/with-random-moves board 100 (board/opposite-colour colour))
          value (board/score future-board ai-colour)]
      (->Node parent board colour pos 1 value nil))))

(def total-count (object-array [0]))

(defn expand-leaf [node ai-colour]
  (let [nodes (make-array 0)
        board (.-board node)
        opposite-colour (board/opposite-colour (.-colour node))]
    (loop [pos 0
           sum 0
           count 0]
      (if (< pos board/max-pos)
        (if (board/valid? board opposite-colour pos)
          (let [leaf (leaf board ai-colour node opposite-colour pos)]
            (.push nodes leaf)
            (recur (inc pos) (+ sum (.-sum leaf)) (inc count)))
          (recur (inc pos) sum count))
        (do (aset total-count 0 (+ (aget total-count 0) count))
            (add-value node ai-colour sum count))))
    (set! (.-nodes node) nodes)))

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

(defn expand [node ai-colour]
  (let [nodes (.-nodes node)]
    (if (nil? nodes)
      (expand-leaf node ai-colour)
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
