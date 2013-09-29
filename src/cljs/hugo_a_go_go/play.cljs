(ns hugo-a-go-go.play
  (:require
   [hugo-a-go-go.client :as client
    :refer [make-move board log width height stone-width padding]]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! alts!]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(def state (atom hugo-a-go-go.client.initial-state))

(defn finished? [state]
  false)

(defn handle [e]
  (let [move (click-to-move e)]
    (log "clicked")
    (log (clj->js move))
    (reset! state (make-move @state move))
    (hugo-a-go-go.client/display @state)
    ;; (let [ai-move (ai @state)]
    ;;   (reset! state (make-move state ai-move))
    ;;   (hugo-a-go-go/display @state))
    ))

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
        x (- x (.-offsetLeft @board))
        y (- y (.-offsetTop @board))

        x (- x padding)
        y (- y padding)


        ]
    [(quot x stone-width)
     (quot y stone-width)]))

(defn human-player [state]
  (let [move (click-to-move (go (<! input-chan)))]
    (make-move state move)))

(defn play-game [p1 p2]
  (let [current-state @state]
    (if (finished? current-state)
      nil
      (do (make-move current-state (p1 state))
          (make-move current-state (p2 state))
          (recur p1 p2)))))

(defn play-random-game []
  ())
