(ns erdos.gogame.core
  (:gen-class)
  (:require [clojure.string :as str]
            [erdos.gogame.board :as board]
            [erdos.gogame.storage :as storage :refer [get-game update-game]]
            [erdos.gogame.templates :as tmpl :refer [->url]]
            [mount.lite :as mount :refer [defstate]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [ring.util.response :refer [content-type redirect resource-response]]
            [s-exp.enso :as enso])
  (:import [java.util.concurrent ThreadLocalRandom]))

(declare config app server) ; linter happy

(defstate config
  :start {:http-port (-> (System/getenv "HTTP_PORT") (or "3000") parse-long)})

(def ^:private id-alphabet "23456789abcdefghjkmnpqrstuvwxyz")

(defn random-id
  "Generates a short, friendly, URL-safe random identifier."
  ([] (random-id 8))
  ([n]
   (let [rnd (ThreadLocalRandom/current)
         len (count id-alphabet)
         sb (StringBuilder. (int n))]
     (dotimes [_ n]
       (.append sb (.charAt id-alphabet (.nextInt rnd len))))
     (str sb))))

(defn create-game!
  ([] (create-game! :standard))
  ([board-type]
   (let [game-id (random-id)
         game {:board (or board-type :standard)
               :black-secret (random-id)
               :white-secret (random-id)
               :history []}]
     (update-game game-id (constantly game))
     (assoc game :game-id game-id))))

(defn player-color [game secret]
  (cond
    (= secret (:black-secret game)) :black
    (= secret (:white-secret game)) :white))

(defn resolve-board-spec [b]
  (if (map? b)
    b
    (get board/boards b (:standard board/boards))))

(def not-found-page
  {:status 404
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (tmpl/not-found-page)})

(defn welcome-page [_req]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (tmpl/welcome-page)})

(defn new-game-handler [{:keys [params] :as req}]
  (let [board-type (-> (:board params)
                       (some-> keyword)
                       (or :standard))
        {:keys [game-id black-secret white-secret]} (create-game! board-type)]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (tmpl/game-created-page {:board-name (-> board-type name str/capitalize)
                                    :board-spec (resolve-board-spec board-type)
                                    :black-url  (->url req :play game-id black-secret)
                                    :white-url  (->url req :play game-id white-secret)})}))

(defn play-page [{:keys [params]} game-id secret]
  (when-let [game (get-game game-id)]
    (when-let [color (player-color game secret)]
      (let [history (:history game)
            board-spec (resolve-board-spec (:board game))
            current-board (board/history->board history board-spec)
            move-numbers (board/history->move-numbers history board-spec)
            turn (board/current-turn history)
            my-turn? (= color turn)
            black-captured (board/captured-count history current-board :white)
            white-captured (board/captured-count history current-board :black)
            error (:error params)
            last2 (take-last 2 history)
            game-over? (and (= 2 (count last2)) (every? :pass last2))
            play-url (->url :play game-id secret)]
        {:status 200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (tmpl/play-page {:game-id game-id
                                :secret secret
                                :color color
                                :board-spec board-spec
                                :current-board current-board
                                :move-numbers move-numbers
                                :history history
                                :my-turn? my-turn?
                                :game-over? game-over?
                                :black-captured black-captured
                                :white-captured white-captured
                                :error error
                                :play-url play-url})}))))


(defn move-handler [{:keys [params]} game-id secret]
  (when-let [game (get-game game-id)]
    (when-let [color (player-color game secret)]
      (let [history (:history game)
            turn (board/current-turn history)
            board-spec (resolve-board-spec (:board game))
            action (:action params)
            x (some-> (:x params) str parse-long)
            y (some-> (:y params) str parse-long)]
        (cond
          (not= color turn)
          (redirect (->url :play game-id secret {:error :turn}) :see-other)

          (= action "pass")
          (do (update-game game-id update :history conj {:color color :pass true})
              (redirect (->url :play game-id secret) :see-other))

          (board/valid-move? board-spec history {:color color :x x :y y})
          (do (update-game game-id update :history conj {:color color :x x :y y})
              (redirect (->url :play game-id secret) :see-other))

          :else
          (redirect (->url :play game-id secret {:error :illegal}) :see-other))))))

(defn handler [{:keys [request-method uri params] :as req}]
  (let [req (assoc req :params (update-keys (or params {}) keyword))
        uri-parts (->> (str/split (or uri "") #"/")
                       (into [] (remove str/blank?)))]
    (condp = [request-method (count uri-parts) (first uri-parts)]
      [:get 0 nil]
      (welcome-page req)

      [:get 1 "style.css"]
      (some-> (resource-response "style.css" {:root "public"})
              (content-type "text/css"))

      [:post 1 "new-game"]
      (new-game-handler req)

      [:get 3 "play"]
      (play-page req (uri-parts 1) (uri-parts 2))

      [:post 3 "play"]
      (move-handler req (uri-parts 1) (uri-parts 2))

      nil)))

(defstate app
  :start (wrap-defaults (some-fn #'handler (constantly not-found-page))
                        (assoc-in site-defaults [:security :anti-forgery] false)))

(defstate server
  :start (enso/run-server @app {:port (:http-port @config)})
  :stop (enso/stop @server))

(defn -main [& _args]
  (mount/start)
  (println (str "Server running on http://localhost:" (:http-port @config)))
  @(promise))
