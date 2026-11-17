(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'erdos/gogame)
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(def uber-file "target/gogame-standalone.jar")
(def native-image-bin "target/gogame")

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [opts]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile '[erdos.gogame.board erdos.gogame.templates erdos.gogame.storage erdos.gogame.core]
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'erdos.gogame.core})
  opts)

(defn native-image [opts]
  (uber opts)
  (let [target-bin (or (:output opts) native-image-bin)
        native-args ["native-image"
                     "-o" target-bin
                     "-jar" uber-file
                     "--no-fallback"
                     "--features=clj_easy.graal_build_time.InitClojureClasses"
                     "-H:IncludeResources=public/.*"
                     "-H:+ReportExceptionStackTraces"]
        res (b/process {:command-args native-args})]
    (when-not (zero? (:exit res))
      (throw (ex-info "Native image compilation failed" res)))
    opts))
