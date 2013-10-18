(ns hugo-a-go-go.tree
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]))

;; TODO value and min/max is probably wrong
;; TODO hashes for detecting repeated positions

(defrecord Node [parent board colour pos count sum nodes valids])

;; TODO should track this incrementally in the board
(defn valids [board colour]
  (let [valids (make-array 0)]
    (dotimes [pos board/max-pos]
      (when (board/valid? board colour pos)
        (.push valids pos)))
    valids))

(defn new [board colour]
  (->Node nil board (board/opposite-colour colour) 0 0 0 (make-array 0) (valids board colour)))

(defn uproot [node]
  (set! (.-parent node) nil)
  (set! (.-pos node) 0))

(defn add-value [node ai-colour value]
  (set! (.-count node) (+ (.-count node) 1))
  (set! (.-sum node) (if (identical? ai-colour (.-colour node))
                       (+ (.-sum node) value)
                       (- (.-sum node) value)))
  (if-let [parent (.-parent node)]
    (recur parent ai-colour value)))

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

(defn expand [node ai-colour]
  (if-let [valid-pos (.pop (.-valids node))]
    (.push (.-nodes node) (expand-leaf (.-board node) ai-colour node (board/opposite-colour (.-colour node)) valid-pos))
    (if-let [child (explorer node)]
      (expand child ai-colour)
      nil ;; no possible moves - pass
      )))

(defn move-for [board colour n]
  (let [node (hugo-a-go-go.tree/new (board/copy board) colour)]
    (dotimes [_ n]
      (expand node colour))
    (when-let [child (exploiter node)]
      (.-pos child))))
