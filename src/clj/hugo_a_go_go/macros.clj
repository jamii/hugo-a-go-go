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

(defmacro get-colour [board pos]
  `(aget ~board ~pos))

(defmacro set-colour [board pos colour]
  `(aset ~board ~pos ~colour))

(defmacro get-string [board pos]
  `(aget ~board (+ ~max-pos ~pos)))

(defmacro set-string [board pos string]
  `(aset ~board (+ ~max-pos ~pos) ~string))

(defmacro get-liberties [board pos]
  `(let [string-ix# (+ ~(+ max-pos max-pos 1) (get-string ~board ~pos))]
     (aget ~board string-ix#)))

(defmacro add-liberties [board pos amount]
  `(let [string-ix# (+ ~(+ max-pos max-pos 1) (get-string ~board ~pos))]
    (aset ~board string-ix# (+ (aget ~board string-ix#) ~amount))))
