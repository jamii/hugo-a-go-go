(ns hugo-a-go-go.board
  (:require-macros [hugo-a-go-go.board-macros :refer [foreach-neighbour]]))

;; ....don't judge me.
;; This was written in a terrible rush

;; TODO death, scoring, cleanup, cljs, graphical

(defrecord String [colour origin size liberties])
(defrecord Board [strings empty-string])

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defn ->pos [x y]
  (+ 1 x (* array-size (+ 1 y))))

(defn new []
  (let [empty-string (->String :empty 0 0 0)
        border-string (->String :grey 0 0 0)
        strings (object-array max-pos)]
    (dotimes [i max-pos]
      (aset strings i empty-string))
    (dotimes [i array-size]
      (aset strings (->pos (dec i) -1) border-string)
      (aset strings (->pos (dec i) size) border-string)
      (aset strings (->pos -1 (dec i)) border-string)
      (aset strings (->pos size (dec i)) border-string))
    (->Board strings empty-string)))

(defn clear-string [^Board board string colour pos]
  (aset (.-strings board) pos (.-empty-string board))
  (foreach-neighbour neighbour-pos pos
                     (let [neighbour-string (aget (.-strings board) neighbour-pos)]
                       (if (identical? string neighbour-string)
                         (clear-string board neighbour-string neighbour-pos)
                         (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))))))

(defn re-string [^Board board from-string to-string pos]
  (when (identical? (aget (.-strings board) pos) from-string)
    (aset (.-strings board) pos to-string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.-strings board) neighbour-pos)]
                         (re-string board from-string to-string neighbour-pos)))))

(defn join-strings [^Board board string-a string-b pos-a pos-b]
  (if (> (.-size string-a) (.-size string-b))
    (join-strings board string-b string-a pos-b pos-a)

    (when-not (identical? string-a string-b)
      (set! (.-size string-b) (+ (.-size string-a) (.-size string-b)))
      (set! (.-liberties string-b) (+ (.-liberties string-a) (.-liberties string-b)))
      (re-string board string-a string-b pos-a))))

(defn get-colour [^Board board pos]
  (.-colour (aget (.-strings board) pos)))

(defn suicide? [^Board board colour pos]
  (let [strings (object-array 4)
        liberties (atom 0)
        string-i (atom 0)]
    (foreach-neighbour neighbour-pos pos
                       (aset strings @string-i (aget (.-strings board) neighbour-pos))
                       (swap! string-i inc))
    (dotimes [i 4]
      (let [string (aget strings i)]
        (when (= colour (.-colour string))
          (swap! liberties + (dec (.-liberties string)))
          (dotimes [j i]
            (when (identical? (aget strings i) (aget strings j))
              (swap! liberties - (.-liberties string)))))
        (when (= :empty (.-colour string))
          (swap! liberties inc))))
    (= 0 @liberties)))

(defn set-colour [^Board board pos colour]
  (assert (= :empty (get-colour board pos)))
  (let [string (->String colour pos 1 0)]
    (aset (.-strings board) pos string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.-strings board) neighbour-pos)
                             neighbour-colour (.-colour neighbour-string)]
                         (case neighbour-colour
                                    :empty (set! (.-liberties (aget (.-strings board) pos)) (inc (.-liberties (aget (.-strings board) pos))))
                                    :grey nil
                                    (do
                                      (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))
                                      (when (= colour neighbour-colour)
                                        (join-strings board (aget (.-strings board) pos) neighbour-string pos neighbour-pos))))))
    ;; TODO check for death
    ))

(defn colour->string [colour]
  (case colour
    :empty "+"
    :black "#"
    :white "O"))

(defn string->colour [string]
  (condp = string
    "+" :empty
    "#" :black
    "O" :white))

(defn print [board]
  (let [strings (into #{} (.-strings board))
        string->id (zipmap strings (map #(char (+ 97 %)) (range)))]
    (prn)
    (dotimes [y size]
      (dotimes [x size]
        (clojure.core/print
         (str (if (and (= :empty (get-colour board (->pos x y))) (suicide? board :black (->pos x y)))
                "X"
                (colour->string (get-colour board (->pos x y))))
              (string->id (aget (.-strings board) (->pos x y)))
              (.-liberties (aget (.-strings board) (->pos x y)))
              " ")))
      (prn))))

(defn debug->board [debug]
  (let [board (hugo-a-go-go.board/new)]
    (doseq [[y row] (zipmap (range) debug)
            [x string] (zipmap (range) row)]
      (when (#{"O" "#"} string)
        (set-colour board (->pos x y) (string->colour string))))
    board))
