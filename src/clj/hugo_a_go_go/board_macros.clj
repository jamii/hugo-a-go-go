(ns hugo-a-go-go.board-macros)

(def size 9)
(def array-size (+ 2 size))

(defmacro foreach-neighbour [sym pos & body]
  (let [pos-sym (gensym "pos")]
    `(let [~pos-sym ~pos]
       ~@(for [neighbour [`(inc ~pos-sym)
                          `(dec ~pos-sym)
                          `(+ ~pos-sym ~array-size)
                          `(- ~pos-sym ~array-size)]]
           `(let [~sym ~neighbour]
              ~@body)))))
