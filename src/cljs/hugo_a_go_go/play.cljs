(ns hugo-a-go-go.play
  (:require
   [hugo-a-go-go.client :as client
    :refer [make-move board-ref log width height stone-width padding]]
   [hugo-a-go-go.tree :as tree]
   [hugo-a-go-go.board :as board]

   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! alts!]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(def state (atom hugo-a-go-go.client.initial-state))

(defn ^:export handle [e]
  (let [move (click-to-move e)]
    (when (board/valid? (:board @state) board/black (apply board/->pos move))
      (log "clicked")
      (log move)
      (reset! state (make-move @state move))
      (hugo-a-go-go.client/display @state)
      (hugo-a-go-go.client/draw-subtitle "Thinking...")
      (js/window.setTimeout
       (fn []
         (let [ai-move (tree/move-for (:board @state) board/white 10000)
               x (dec (mod ai-move board/array-size))
               y (dec (quot ai-move board/array-size))]
           (log [x y])
           (reset! state (make-move @state [x y]))
           (hugo-a-go-go.client/display @state)
           (hugo-a-go-go.client/draw-subtitle (board/score (:board @state)))
           (hugo-a-go-go.client/highlight-move ai-move)
           ))
       1))))

(defn click-to-move [e]
  (let [x (if (.-pageX e)
            (.-pageX e)
            (+ (.-clientX e)
               (-> js/document
                   (.-body)
                   (.-scrollLeft))
               (-> js/document
                   (.-documentElement)
                   (.-scrollLeft))))
        y (if (.-pageY e)
            (.-pageY e)
            (+ (.-clientY e)
               (-> js/document
                   (.-body)
                   (.-scrollTop))
               (-> js/document
                   (.-documentElement)
                   (.-scrollTop))))
        x (- x (.-offsetLeft @board-ref))
        y (- y (.-offsetTop @board-ref))

        x (- x padding)
        y (- y padding)


        ]
    [(quot x stone-width)
     (quot y stone-width)]))
