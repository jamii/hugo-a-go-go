(ns hugo-a-go-go.play
  (:require
   [hugo-a-go-go.client :as client
    :refer [make-move log]]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! alts!]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(def state (atom hugo-a-go-go.client.initial-state))
(def input-chan (chan))

(defn finished? [state]
  false)

(defn handle [e]
  ;; (if (= (:to-move @state) :black)
  (log "clicked")
  (log e)
  (put! input-chan  e))
;; )

(defn click-to-move [e]
  (log e))

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
  (go (loop []
        (log (<! input-chan))
        (recur))))
