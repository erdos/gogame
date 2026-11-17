(ns erdos.gogame.storage
  (:require [clojure.string :as str]
            [mount.lite :refer [defstate]])
  (:import [org.h2.mvstore MVMap MVStore MVStore$Builder]))

(declare config store games)

(defstate config
  :start (let [f (System/getenv "DB_FILE")]
           (when (str/blank? f)
             (throw (ex-info "Missing required environment variable DB_FILE!" {:env "DB_FILE"})))
           f))

(defstate store
  :start (-> (MVStore$Builder.)
             (.fileName @config)
             (.compress)
             (.open))
  :stop (.close ^MVStore @store))

(defstate games
  :start (.openMap ^MVStore @store "games"))

(defn update-game
  "Updates the game at game-key by applying fun to its current value (nil if non-existent) and args.
   Saves and commits the new value into the games MVMap and returns the new value."
  [game-key fun & args]
  (locking @store
    (let [val (get @games game-key)
          new-val (apply fun val args)]
      (.put ^MVMap @games game-key new-val)
      (.commit ^MVStore @store)
      new-val)))

(defn get-game [game-key]
  (get @games game-key))
