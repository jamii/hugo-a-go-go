(ns hugo-a-go-go.tree
  (:require [hugo-a-go-go.board :as board]
            [hugo-a-go-go.random :as random]))

;; TODO value and min/max is probably wrong

(defrecord Node [parent colour pos count sum nodes valids])

(defn value [board colour]
  (let [score (board/score board)]
    (- (score colour) (score (board/opposite-colour colour)))))

;; TODO should track this incrementally in the board
(defn valids [board colour]
  (let [valids (object-array 0)]
    (dotimes [pos board/max-pos]
      (when (board/valid? board colour pos)
        (.push valids pos)))
    valids))

(defn new []
  (->Node nil :black 0 0 0 (object-array 0) (valids (board/new) :black)))

(defn uproot [node]
  (set! (.-parent node) nil)
  (set! (.-pos node) 0))

(defn add-value [node value]
  (set! (.-count node) (+ (.-count node) 1))
  (set! (.-sum node) (+ (.-sum node) value))
  (if-let [parent (.-parent node)]
    (recur parent value)))

(defn expand-leaf [board parent colour pos]
  (board/set-colour board pos colour)
  (let [valids (valids board colour)]
    (random/with-random-moves board 100 (board/opposite-colour colour))
    (let [value (value board colour)]
      (add-value parent value)
      (->Node parent colour pos 1 value (object-array 0) valids))))

(defn best-child [node]
  (let [best-score (atom (- (/ 1 0)))
        best-child (atom nil)]
    (doseq [child (.-nodes node)]
      (let [score (+ (/ (.-sum child) (.-count child))
                     (js/Math.sqrt
                      (/ (* 2 (js/Math.log (.-count node)))
                         (* 5 (.-count child)))))]
        (when (> score @best-score)
          (reset! best-score score)
          (reset! best-child child))))
    @best-child))

(defn expand
  ([node]
     (expand (board/new) node))
  ([board node]
     (let [pos (.-pos node)]
       (if (not= 0 pos) ;; top node has pos 0 - probably a smell
         (board/set-colour board pos (.-colour node))))
     (if-let [empty-pos (.pop (.-valids node))]
       (.push (.-nodes node) (expand-leaf board node (board/opposite-colour (.-colour node)) empty-pos))
       (if-let [child (best-child node)]
         (expand board child)))))
