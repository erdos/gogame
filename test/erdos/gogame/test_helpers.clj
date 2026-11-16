(ns erdos.gogame.test-helpers
  (:require [erdos.gogame.storage :as storage]
            [mount.lite :as mount]))

(defn storage-fixture [f]
  (mount/start #'storage/games)
  (try
    (f)
    (finally
      (mount/stop))))

(defn clear-games! []
  (locking @storage/store
    (.clear @storage/games)
    (.commit @storage/store)))
