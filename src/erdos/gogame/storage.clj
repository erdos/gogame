(ns erdos.gogame.storage
  (:require [clojure.string :as str]
            [mount.lite :refer [defstate]])
  (:import [org.h2.mvstore MVStore$Builder]))

(declare config store games)

(defstate config
  :start (let [f (System/getenv "DB_FILE")]
           (when-not (str/blank? f) f)))

(defstate store
  :start (let [builder (cond-> (MVStore$Builder.)
                         @config (.fileName @config)
                         true (.compress))]
           (.open builder))
  :stop (.close @store))

(defstate games
  :start (.openMap @store "games"))

(defn update-game
  "Updates the game at game-key by applying fun to its current value (nil if non-existent) and args.
   Saves and commits the new value into the games MVMap and returns the new value."
  [game-key fun & args]
  (locking @store
    (let [val (get @games game-key)
          new-val (apply fun val args)]
      (.put @games game-key new-val)
      (.commit @store)
      new-val)))

(defn get-game [game-key]
  (get @games game-key))
