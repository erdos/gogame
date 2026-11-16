(ns erdos.gogame.test-helpers
  (:require [erdos.gogame.storage :as storage]
            [mount.lite :as mount]))

(defn storage-fixture [f]
  (let [db-file (java.io.File/createTempFile "gogame-test" ".mv")]
    (.delete db-file)
    (mount/with-substitutes [#'storage/config (mount/state :start (.getPath db-file))]
      (mount/start #'storage/games))
    (try
      (f)
      (finally
        (mount/stop)
        (.delete db-file)))))

(defn clear-games! []
  (locking @storage/store
    (.clear @storage/games)
    (.commit @storage/store)))
