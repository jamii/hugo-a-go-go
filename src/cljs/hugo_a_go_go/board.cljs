(ns hugo-a-go-go.board
  (:require-macros [hugo-a-go-go.board-macros :refer [foreach-neighbour]]))

;; ....don't judge me.
;; This was written in a terrible rush

;; TODO scoring, cleanup

(defrecord String [colour origin size liberties])
(defrecord Board [strings empty-string])

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defn ->pos [x y]
  (+ 1 x (* array-size (+ 1 y))))

(defn opposite-colour [colour]
  (condp keyword-identical? colour
    :black :white
    :white :black))

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
                         (clear-string board neighbour-string neighbour-colour neighbour-pos)
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
  (condp = i
    0 (- pos array-size)
    1 (inc pos)
    2 (+ pos array-size)
    3 (dec pos)))

(defn suicide? [^Board board colour pos]
  (let [suicide (atom true)
        opposite-colour (condp keyword-identical? colour :black :white :white :black)]
    (dotimes [i 4]
      (let [string (aget (.-strings board) (neighbour pos i))]
        (set! (.-liberties string) (dec (.-liberties string)))))
    (dotimes [i 4]
      (let [string (aget (.-strings board) (neighbour pos i))]
        (condp keyword-identical? (.-colour string)
          colour (when (> (.-liberties string) 0)
                   (reset! suicide false))
          opposite-colour (when (= (.-liberties string) 0)
                            (reset! suicide false))
          :empty (reset! suicide false)
          :grey nil)))
    (dotimes [i 4]
      (let [string (aget (.-strings board) (neighbour pos i))]
        (set! (.-liberties string) (inc (.-liberties string)))))
    @suicide))

(defn valid? [^Board board colour pos]
  (and (keyword-identical? :empty (get-colour board pos))
       (not (suicide? board colour pos))))

(defn set-colour [^Board board pos colour]
  (let [string (->String colour pos 1 0)]
    (aset (.-strings board) pos string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.-strings board) neighbour-pos)
                             neighbour-colour (.-colour neighbour-string)]
                         (condp keyword-identical? neighbour-colour
                           :empty
                           (set! (.-liberties (aget (.-strings board) pos)) (inc (.-liberties (aget (.-strings board) pos))))

                           :grey
                           nil

                           colour
                           (do
                             (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))
                             (join-strings board (aget (.-strings board) pos) neighbour-string pos neighbour-pos))

                           ;; opposite colour
                           (do
                             (set! (.-liberties neighbour-string) (dec (.-liberties neighbour-string)))
                             (when (= 0 (.-liberties neighbour-string))
                               (clear-string board neighbour-string neighbour-colour neighbour-pos))))))))

;; TODO this is a really dumb solution
(defn copy [board]
  (let [new-board (hugo-a-go-go.board/new)]
    (dotimes [pos max-pos]
      (let [colour (get-colour board pos)]
        (when (#{:black :white} colour)
          (set-colour new-board pos colour))))
    new-board))

(defn flood-fill [board colour]
  (let [filled (object-array max-pos)]
    (letfn [(flood-fill-around [pos]
              (dotimes [i 4]
                (let [pos (neighbour pos i)]
                  (when (and (not (aget filled pos))
                             (keyword-identical? :empty (get-colour board pos)))
                    (aset filled pos true)
                    (flood-fill-around pos)))))]
      (dotimes [x size]
        (dotimes [y size]
          (let [pos (->pos x y)]
            (when (keyword-identical? colour (get-colour board pos))
              (aset filled pos true)
              (flood-fill-around pos))))))
    (count (filter identity filled))))

(defn score [board]
  (let [white-flood (flood-fill board :white)
        black-flood (flood-fill board :black)
        total (* size size)
        overlap (- (+ white-flood black-flood) total)
        white-score (- white-flood overlap)
        black-score (- black-flood overlap)]
    {:white white-score :black black-score}))

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
