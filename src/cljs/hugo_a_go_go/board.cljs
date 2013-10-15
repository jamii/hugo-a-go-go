(ns hugo-a-go-go.board
  (:require-macros [hugo-a-go-go.board-macros :refer [foreach-neighbour]]))

;; ....don't judge me.
;; This was written in a terrible rush

;; TODO scoring, cleanup
;; TODO could use (identical? empty-string %) instead of (= :empty %)

(defrecord String [colour origin size liberties])
(defrecord Board [strings empty-string])

(def black :black)
(def white :white)
(def grey :grey)
(def empty :empty)

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defn ->pos [x y]
  (+ 1 x (* array-size (+ 1 y))))

(defn opposite-colour [colour]
  (if (identical? colour black) white black))

(defn new []
  (let [empty-string (->String empty 0 0 0)
        border-string (->String grey 0 0 0)
        strings (object-array max-pos)]
    (dotimes [i max-pos]
      (aset strings i empty-string))
    (dotimes [i array-size]
      (aset strings (->pos (dec i) -1) border-string)
      (aset strings (->pos (dec i) size) border-string)
      (aset strings (->pos -1 (dec i)) border-string)
      (aset strings (->pos size (dec i)) border-string))
    (->Board strings empty-string)))

(defn clear-string [^Board board string pos]
  (aset (.-strings board) pos (.-empty-string board))
  (foreach-neighbour neighbour-pos pos
                     (let [neighbour-string (aget (.-strings board) neighbour-pos)]
                       (if (identical? string neighbour-string)
                         (clear-string board neighbour-string neighbour-pos)
                         (set! (.-liberties neighbour-string) (inc (.-liberties neighbour-string)))))))

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

(defn get-liberties [^Board board pos]
  (.-liberties (aget (.-strings board) pos)))

(defn neighbour [pos i]
  (condp == i
    0 (- pos array-size)
    1 (inc pos)
    2 (+ pos array-size)
    3 (dec pos)))

(defn ^boolean suicide? [^Board board colour pos]
  (let [suicide (object-array 1)
        _ (aset suicide 0 true)
        opposite-colour (opposite-colour colour)]
    (foreach-neighbour neighbour-pos pos
      (let [string (aget (.-strings board) neighbour-pos)]
        (set! (.-liberties string) (dec (.-liberties string)))))
    (foreach-neighbour neighbour-pos pos
      (let [string (aget (.-strings board) neighbour-pos)]
        (cond
         (identical? (.-colour string) colour)
         (when (> (.-liberties string) 0)
           (aset suicide 0 false))

         (identical? (.-colour string) opposite-colour)
         (when (== (.-liberties string) 0)
           (aset suicide 0 false))

         (identical? (.-colour string) empty)
         (aset suicide 0 false))))
    (foreach-neighbour neighbour-pos pos
      (let [string (aget (.-strings board) neighbour-pos)]
        (set! (.-liberties string) (inc (.-liberties string)))))
    (aget suicide 0)))

(defn ^boolean eyelike? [^Board board colour pos]
  (let [eyelike? (object-array 1)
        _ (aset eyelike? 0 true)]
    (foreach-neighbour neighbour-pos pos
                       (when (not (identical? colour (get-colour board neighbour-pos)))
                         (aset eyelike? 0 false)))
    (aget eyelike? 0)))

(defn ^boolean empty? [board pos]
  (identical? (.-empty-string board) (aget (.-strings board) pos)))

(defn ^boolean valid? [^Board board colour pos]
  (and (empty? board pos)
       (not (suicide? board colour pos))))

(defn set-colour [^Board board pos colour]
  (let [string (->String colour pos 1 0)]
    (aset (.-strings board) pos string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.-strings board) neighbour-pos)
                             neighbour-colour (.-colour neighbour-string)]
                         (cond
                          (identical? neighbour-colour empty)
                          (set! (.-liberties (aget (.-strings board) pos)) (inc (.-liberties (aget (.-strings board) pos))))

                          (identical? neighbour-colour colour)
                          (do
                            (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))
                            (join-strings board (aget (.-strings board) pos) neighbour-string pos neighbour-pos))

                          (identical? neighbour-colour (opposite-colour colour))
                          (do
                            (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))
                            (when (== 0 (.-liberties neighbour-string))
                               (clear-string board neighbour-string neighbour-pos))))))))

;; TODO this is a really dumb solution
(defn copy [board]
  (let [new-board (hugo-a-go-go.board/new)]
    (dotimes [pos max-pos]
      (let [colour (get-colour board pos)]
        (when (or (identical? white colour) (identical? black colour))
          (set-colour new-board pos colour))))
    new-board))

(defn flood-fill [board colour]
  (let [filled (object-array max-pos)]
    (letfn [(flood-fill-around [pos]
              (foreach-neighbour pos pos
                  (when (and (not (aget filled pos))
                             (empty? board pos))
                    (aset filled pos true)
                    (flood-fill-around pos))))]
      (dotimes [x size]
        (dotimes [y size]
          (let [pos (->pos x y)]
            (when (identical? colour (get-colour board pos))
              (aset filled pos true)
              (flood-fill-around pos))))))
    (areduce filled i sum 0 (if (aget filled i) (inc sum) sum))))

(defn score [board]
  (let [white-flood (flood-fill board white)
        black-flood (flood-fill board black)
        total (* size size)
        overlap (- (+ white-flood black-flood) total)
        white-score (- white-flood overlap)
        black-score (- black-flood overlap)]
    {white white-score black black-score}))

(defn colour->string [colour]
  (case colour
    empty "+"
    black "#"
    white "O"))

(defn string->colour [string]
  (condp = string
    "+" empty
    "#" black
    "O" white))

(defn print [board]
  (let [strings (into #{} (.-strings board))
        string->id (zipmap strings (map #(char (+ 97 %)) (range)))]
    (prn)
    (dotimes [y size]
      (dotimes [x size]
        (clojure.core/print
         (str (if (and (= empty (get-colour board (->pos x y))) (suicide? board black (->pos x y)))
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
