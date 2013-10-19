(ns hugo-a-go-go.board
  (:require-macros [hugo-a-go-go.macros :refer [case== neighbour foreach-neighbour get-colour set-colour get-string set-string get-neighbours add-neighbours get-liberties add-liberties new-string]]))

;; ....don't judge me.
;; This was written in a terrible rush

;; --- COLOURS ---

(def empty 0)
(def black 1)
(def white 2)
(def grey 3)

(defn opposite-colour [colour]
  (if (== colour black) white black))

;; --- LAYOUT ---

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defn ->pos [x y]
  (+ 1 x (* array-size (+ 1 y))))

(def empty-string 0)
(def grey-string 1)

(defn new []
  (let [board (new js/Uint8Array 1024)
        empty-string (new-string board)
        grey-string (new-string board)]
    ;; colours and strings all start as 0 = empty
    (dotimes [i array-size]
      (set-colour board (->pos (dec i) -1) grey)
      (set-string board (->pos (dec i) -1) grey-string)
      (add-neighbours board (->pos (dec i) 0) 1)

      (set-colour board (->pos (dec i) size) grey)
      (set-string board (->pos (dec i) size) grey-string)
      (add-neighbours board (->pos (dec i) (dec size)) 1)

      (set-colour board (->pos -1 (dec i)) grey)
      (set-string board (->pos -1 (dec i)) grey-string)
      (add-neighbours board (->pos 0 (dec i)) 1)

      (set-colour board (->pos size (dec i)) grey)
      (set-string board (->pos size (dec i)) grey-string)
      (add-neighbours board (->pos (dec size) (dec i)) 1))
    board))

(defn copy [board]
  (new js/Uint8Array board))

;; --- STONES AND STRINGS ---

(defn clear-stone [board pos]
  (set-colour board pos empty)
  (set-string board pos empty-string)
  (foreach-neighbour pos neighbour-pos
                     (add-neighbours board neighbour-pos -1)
                     (add-liberties board neighbour-pos 1)))

(defn clear-string [board string pos]
  (clear-stone board pos)
  (foreach-neighbour pos neighbour-pos
                     (when (== string (get-string board neighbour-pos))
                       (clear-string board string neighbour-pos))))

(defn rename-string [board from-string to-string pos]
  (when (== from-string (get-string board pos))
    (set-string board pos to-string)
    (foreach-neighbour pos neighbour-pos
                       (rename-string board from-string to-string neighbour-pos))))

(defn join-strings [board pos-a pos-b]
  (add-liberties board pos-b (get-liberties board pos-a))
  (rename-string board (get-string board pos-a) (get-string board pos-b) pos-a))

(defn place-stone [board pos colour]
  (let [string (new-string board)
        opposite-colour (opposite-colour colour)]
    (set-colour board pos colour)
    (set-string board pos string)
    (foreach-neighbour pos neighbour-pos
                       (add-neighbours board neighbour-pos 1)
                       (case== (get-colour board neighbour-pos)
                               empty (add-liberties board pos 1)
                               colour (do (add-liberties board neighbour-pos -1)
                                          (when (not (== (get-string board pos) (get-string board neighbour-pos)))
                                            (join-strings board pos neighbour-pos)))
                               opposite-colour (do (add-liberties board neighbour-pos -1)
                                                   (when (== 0 (get-liberties board neighbour-pos))
                                                     (clear-string board (get-string board neighbour-pos) neighbour-pos)))))))

;; --- INFO ---

(def suicide-box (make-array 1))

(defn ^boolean suicide? [board colour pos]
  (if (< (get-neighbours board pos) 4)
    false
    (let [opposite-colour (opposite-colour colour)]
      (aset suicide-box 0 true)
      (foreach-neighbour pos neighbour-pos

                         (add-liberties board neighbour-pos -1))
      (foreach-neighbour pos neighbour-pos

                         (case== (get-colour board neighbour-pos)
                                 colour (when (> (get-liberties board neighbour-pos) 0)
                                          (aset suicide-box 0 false))
                                 opposite-colour (when (== (get-liberties board neighbour-pos) 0)
                                                   (aset suicide-box 0 false))))
      (foreach-neighbour pos neighbour-pos

                         (add-liberties board neighbour-pos 1))
      (aget suicide-box 0))))

(defn ^boolean eyelike? [board colour pos]
  (if (< (get-neighbours board pos) 4)
    false
    (let [above (get-string board (neighbour pos 0))]
      (and (== above (get-string board (neighbour pos 1)))
           (== above (get-string board (neighbour pos 2)))
           (== above (get-string board (neighbour pos 3)))))))

(defn ^boolean empty? [board pos]
  (== empty (get-colour board pos)))

(defn ^boolean valid? [board colour pos]
  (and (empty? board pos)
       (not (suicide? board colour pos))))

;; --- SCORING ---

(defn flood-fill-around [board filled pos]
  (when (and (== 0 (aget filled pos))
             (empty? board pos))
    (aset filled pos 1)
    (foreach-neighbour pos neighbour-pos
                       (flood-fill-around board filled neighbour-pos))))

(defn flood-fill [board colour]
  (let [filled (js/Int8Array. max-pos)]
    (dotimes [pos max-pos]
      (when (== colour (get-colour board pos))
        (aset filled pos 1)
        (foreach-neighbour pos neighbour-pos
                           (flood-fill-around board filled neighbour-pos))))
    filled))

(defn score [board colour]
  (let [colour-filled (flood-fill board colour)
        opposite-colour-filled (flood-fill board (opposite-colour colour))]
    (areduce colour-filled i sum 0
             (if (and (== 1 (aget colour-filled i)) (== 0 (aget opposite-colour-filled i)))
               (inc sum)
               sum))))

;; --- DEBUGGING

(defn colour->string [colour]
  (case== colour
          empty " "
          black "#"
          white "O"
          grey "+"))

(defn string->colour [string]
  (condp = string
    " " empty
    "#" black
    "O" white
    "+" grey))

(defn board->string [board]
  (with-out-str
    (let [strings (into #{} (.-strings board))
          string->id (zipmap strings (map #(char (+ 97 %)) (range)))]
      (dotimes [y array-size]
        (prn)
        (dotimes [x array-size]
          (let [pos (+ y (* array-size x))]
            (print
             (str (if (and (empty? board pos)
                           (suicide? board black pos))
                    "X"
                    (colour->string (get-colour board pos)))
                  (char (+ 97 (get-string board pos)))
                  (get-liberties board pos)
                  " ")))))
      (prn))))

(defn debug->board [debug]
  (let [board (hugo-a-go-go.board/new)]
    (doseq [[y row] (zipmap (range) debug)
            [x string] (zipmap (range) row)]
      (when (#{"O" "#"} string)
        (place-stone board (->pos x y) (string->colour string))))
    board))
