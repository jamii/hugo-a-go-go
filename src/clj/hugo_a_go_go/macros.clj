(ns hugo-a-go-go.macros)

(def size 9)
(def array-size (+ 2 size))
(def max-pos (* array-size array-size))

(defmacro case== [value & cases&bodies]
  (let [value-sym (gensym "value")]
    `(let [~value-sym ~value]
       (cond
        ~@(interleave
           (for [case (take-nth 2 cases&bodies)]
             `(== ~value-sym ~case))
           (take-nth 2 (rest cases&bodies)))
        ~@(when (odd? (count cases&bodies))
            [true (last cases&bodies)])))))


;; board layout:
;; pos->colour * max-pos
;; pos->string * max-pos
;; pos->neighbours * max-pos
;; string->liberties * ?
;; next-string * 1

(defmacro get-colour [board pos]
  `(aget ~board ~pos))

(defmacro set-colour [board pos colour]
  `(aset ~board ~pos ~colour))

(defmacro get-string [board pos]
  `(aget ~board (+ ~max-pos ~pos)))

(defmacro set-string [board pos string]
  `(aset ~board (+ ~max-pos ~pos) ~string))

(defmacro get-neighbours [board pos]
  `(let [freedom-ix# (+ ~(* 2 max-pos) ~pos)]
     (aget ~board freedom-ix#)))

(defmacro add-neighbours [board pos amount]
  `(let [freedom-ix# (+ ~(* 2 max-pos) ~pos)]
     (aset ~board freedom-ix# (+ (aget ~board freedom-ix#) ~amount))))

(defmacro get-liberties [board pos]
  `(let [string-ix# (+ ~(* 3 max-pos) (get-string ~board ~pos))]
     (aget ~board string-ix#)))

(defmacro add-liberties [board pos amount]
  `(let [string-ix# (+ ~(* 3 max-pos) (get-string ~board ~pos))]
     (aset ~board string-ix# (+ (aget ~board string-ix#) ~amount))))

(defmacro new-string [board]
  `(let [next-string# (aget ~board 1023)]
     (aset ~board 1023 (+ next-string# 1))
     next-string#))
