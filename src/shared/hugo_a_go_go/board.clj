(ns hugo-a-go-go.board)

;; ....don't judge me.
;; This was written in a terrible rush

(defmacro defenum [syms]
  `(do ~@(for [[sym i] (zipmap syms (range))]
           `(def ~sym ~i))))

(defmacro enum-case [value & clauses]
  `(case ~value
     ~@(apply concat
              (for [[comp branch] (partition 2 clauses)]
                [(eval comp) branch]))
     ~@(if (odd? (count clauses)) [(last clauses)] [])))

(defmacro defstruct [name syms]
  `(do (defn ~(symbol (str "->" name)) [& args#]
         (byte-array ~(count syms) (map unchecked-byte args#)))
       ~@(for [[sym i] (zipmap syms (range))]
           `(do
              (defn ~(symbol (str "get-" (.toLowerCase (str name)) "-" sym)) [struct#]
                (aget struct# ~i))
              (defn ~(symbol (str "set-" (.toLowerCase (str name)) "-" sym)) [struct# value#]
                (aset struct# ~i (unchecked-byte value#)))))))

(defenum [empty black white grey])
(defstruct String [colour origin size liberties])
(defrecord Board [strings empty-string])

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defn ->pos [x y]
  (+ 1 x (* array-size (+ 1 y))))

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

(defmacro foreach-neighbour [sym pos & body]
  (let [pos-sym (gensym "pos")]
    `(let [~pos-sym ~pos]
       ~@(for [neighbour [`(inc ~pos-sym) `(dec ~pos-sym) `(+ ~pos-sym array-size) `(- ~pos-sym array-size)]]
           `(let [~sym ~neighbour]
              ~@body)))))

(defn clear-string [^Board board string colour pos]
  (aset (.strings board) pos (.empty-string board))
  (foreach-neighbour neighbour-pos pos
                     (let [neighbour-string (aget (.strings board) neighbour-pos)]
                       (if (identical? string neighbour-string)
                         (clear-string board neighbour-string neighbour-pos)
                         (set-string-liberties neighbour-string (dec (get-string-liberties neighbour-string)))))))

(defn re-string [^Board board from-string to-string pos]
  (when (identical? (aget (.strings board) pos) from-string)
    (aset (.strings board) pos to-string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.strings board) neighbour-pos)]
                         (re-string board from-string to-string neighbour-pos)))))

(defn join-strings [^Board board string-a string-b pos-a pos-b]
  (if (> (get-string-size string-a) (get-string-size string-b))
    (join-strings board string-b string-a pos-b pos-a)

    (when-not (identical? string-a string-b)
      (set-string-size string-b (+ (get-string-size string-a) (get-string-size string-b)))
      (set-string-liberties string-b (+ (get-string-liberties string-a) (get-string-liberties string-b)))
      (re-string board string-a string-b pos-a))))

(defn get-colour [^Board board pos]
  (get-string-colour (aget (.strings board) pos)))

(defn suicide? [^Board board colour pos]
  (let [strings (object-array 4)
        liberties (atom 0)
        string-i (atom 0)]
    (foreach-neighbour neighbour-pos pos
                       (aset strings @string-i (aget (.strings board) neighbour-pos))
                       (swap! string-i inc))
    (dotimes [i 4]
      (let [string (aget strings i)]
        (when (= colour (get-string-colour string))
          (swap! liberties + (dec (get-string-liberties string)))
          (dotimes [j i]
            (when (identical? (aget strings i) (aget strings j))
              (swap! liberties - (get-string-liberties string)))))
        (when (= empty (get-string-colour string))
          (swap! liberties inc))))
    (= 0 @liberties)))

(defn set-colour [^Board board pos colour]
  (assert (= empty (get-colour board pos)))
  (let [string (->String colour pos 1 0)]
    (aset (.strings board) pos string)
    (foreach-neighbour neighbour-pos pos
                       (let [neighbour-string (aget (.strings board) neighbour-pos)
                             neighbour-colour (get-string-colour neighbour-string)]
                         (enum-case neighbour-colour
                                    empty (set-string-liberties (aget (.strings board) pos) (inc (get-string-liberties (aget (.strings board) pos))))
                                    grey nil
                                    (do
                                      (set-string-liberties neighbour-string (dec (get-string-liberties neighbour-string)))
                                      (when (= colour neighbour-colour)
                                        (join-strings board (aget (.strings board) pos) neighbour-string pos neighbour-pos))))))
    ;; TODO check for death
    ))

(defn colour->string [colour]
  (enum-case colour
    empty "+"
    black "#"
    white "O"))

(defn string->colour [string]
  (condp = string
    "+" empty
    "#" black
    "O" white))

(defn print [board]
  (let [strings (into #{} (.strings board))
        string->id (zipmap strings (map #(char (+ 97 %)) (range)))]
    (prn)
    (dotimes [y size]
      (dotimes [x size]
        (clojure.core/print
         (str (if (and (= empty (get-colour board (->pos x y))) (suicide? board black (->pos x y)))
                "X"
                (colour->string (get-colour board (->pos x y))))
              (string->id (aget (.strings board) (->pos x y)))
              (get-string-liberties (aget (.strings board) (->pos x y)))
              " ")))
      (prn))))

(defn debug->board [debug]
  (let [board (hugo-a-go-go.board/new)]
    (doseq [[y row] (zipmap (range) debug)
            [x string] (zipmap (range) row)]
      (when (#{"O" "#"} string)
        (set-colour board (->pos x y) (string->colour string))
        (print board)))
    board))

(prn "---------------------------------")
(debug->board [["O" "#" "#" "O"]
               ["+" "+" "O" "O"]
               ["O" "O" "O" "#"]
               ["#" "#" "O" "+"]
               ["#" "+" "O" "+"]
               ["O" "#" "#" "O"]
               ["O" "O" "O" "O"]])

(comment
  (def board (hugo-a-go-go.board/new))
  (set-colour board (->pos 3 3) black)
  (set-colour board (->pos 3 4) white)
  (set-colour board (->pos 0 0) black)
  (get-colour board (->pos 8 8))
  (print board)

  (def string (->String black 0 0 0))
  (set-string-liberties string 3)
  (get-string-liberties string)
  )
