(ns erdos.gogame.handler-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [erdos.gogame.core :as core :refer [create-game!]]
            [erdos.gogame.storage :as storage :refer [config games get-game
                                                      store update-game]]
            [erdos.gogame.test-helpers :refer [clear-games! storage-fixture]]
            [erdos.gogame.templates :as tmpl :refer [->url]]
            [hiccup.core :as hiccup]
            [matcher-combinators.test :refer [match?]]
            [mount.lite :as mount]
            [ring.util.response :as resp])
  (:import [java.net URL URLStreamHandler]
           [org.h2.mvstore MVStore MVMap MVStore$Builder]))

(use-fixtures :once storage-fixture)

(deftest url-test
  (testing "->url without req constructs path starting with /"
    (is (= "/" (->url)))
    (is (= "/" (->url nil)))
    (is (= "/" (->url nil "")))
    (is (= "/" (->url nil "/")))
    (is (= "/play/123/abc" (->url "play" "123" "abc")))
    (is (= "/play/123/abc" (->url :play "123" "abc")))
    (is (= "/play/123/abc" (->url nil "play" "123" "abc")))
    (is (= "/play/123/abc" (->url nil "/play" "123" "abc"))))

  (testing "(->url :play ...) is identical to (->url nil \"play\" ...)"
    (is (= (->url :play "123" "abc")
           (->url nil "play" "123" "abc"))))

  (testing "->url with req containing host constructs full URL"
    (is (= "http://localhost:3000/play/123/abc"
           (->url {:headers {"host" "localhost:3000"}} :play "123" "abc")))
    (is (= "https://example.com/play/123/abc"
           (->url {:scheme :https :headers {"host" "example.com"}} :play "123" "abc"))))

  (testing "->url with req lacking host falls back to relative path"
    (is (= "/play/123/abc"
           (->url {:request-method :get} :play "123" "abc"))))

  (testing "->url calls name on keywords and symbols"
    (is (= "/play/123/abc" (->url :play "123" "abc")))
    (is (= "/play/123/abc" (->url 'play "123" "abc"))))

  (testing "->url serializes map as query params"
    (is (= "/play/123/abc?error=turn"
           (->url :play "123" "abc" {:error :turn})))
    (is (= "/play/123/abc?error=turn"
           (->url :play "123" "abc" {:error "turn"})))
    (is (= "/play/123/abc"
           (->url :play "123" "abc" {})))
    (is (= "http://localhost:3000/play/123/abc?error=illegal"
           (->url {:headers {"host" "localhost:3000"}} :play "123" "abc" {:error :illegal})))))

(deftest random-id-test
  (testing "random-id generates short, friendly IDs with default length 8"
    (let [id (core/random-id)]
      (is (= 8 (count id)))
      (is (re-matches #"^[2-9a-km-np-z]{8}$" id))))

  (testing "random-id accepts custom length"
    (is (= 6 (count (core/random-id 6))))
    (is (= 12 (count (core/random-id 12)))))

  (testing "random-id produces distinct values"
    (let [ids (repeatedly 50 core/random-id)]
      (is (= 50 (count (set ids)))))))

(deftest css-resource-test
  (testing "GET /style.css returns the CSS resource stylesheet"
    (let [resp (core/handler {:request-method :get :uri "/style.css"})]
      (is (= 200 (:status resp)))
      (is (= "text/css" (get-in resp [:headers "Content-Type"])))
      (is (str/includes? (slurp (:body resp)) ".go-board"))))

  (testing "Pages link to /style.css"
    (let [resp (core/handler {:request-method :get :uri "/"})]
      (is (str/includes? (:body resp) "/style.css"))
      (is (str/includes? (:body resp) "stylesheet"))))

  (testing "resource-data for :resource protocol rejects directory URLs ending in /"
    (let [dummy-handler (proxy [URLStreamHandler] []
                          (openConnection [_] nil))
          dir-url (URL. "resource" "" -1 "/public/" dummy-handler)]
      (is (nil? (resp/resource-data dir-url))))))

(deftest welcome-page-test
  (testing "GET / returns welcome page with New Game form and size dropdown"
    (let [resp (core/handler {:request-method :get :uri "/"})]
      (is (match? {:status 200
                   :headers {"Content-Type" "text/html; charset=utf-8"}}
                  resp))
      (is (str/includes? (:body resp) "New Game"))
      (is (str/includes? (:body resp) "action=\"/new-game\""))
      (is (str/includes? (:body resp) "select"))
      (is (str/includes? (:body resp) "Standard (19x19)"))
      (is (str/includes? (:body resp) "Intermediate (13x13)"))
      (is (str/includes? (:body resp) "Beginner (9x9)")))

    (testing "GET / through app middleware returns HTML welcome page"
      (mount/start #'core/app)
      (let [resp (@core/app {:request-method :get :uri "/"})]
        (is (= 200 (:status resp)))
        (is (str/includes? (get-in resp [:headers "Content-Type"]) "text/html"))
        (is (str/includes? (:body resp) "New Game"))))))

(deftest create-game-test
  (testing "POST /new-game creates a new game and shows distinct links"
    (clear-games!)
    (let [resp (core/handler {:request-method :post :uri "/new-game"})]
      (is (match? {:status 200
                   :headers {"Content-Type" "text/html; charset=utf-8"}}
                  resp))
      (is (= 1 (count @games)))
      (let [[game-id game] (first @games)]
        (is (match? {:board :standard
                     :black-secret string?
                     :white-secret string?
                     :history []}
                    game))
        (is (re-matches #"^[2-9a-km-np-z]{8}$" game-id))
        (is (re-matches #"^[2-9a-km-np-z]{8}$" (:black-secret game)))
        (is (re-matches #"^[2-9a-km-np-z]{8}$" (:white-secret game)))
        (is (not= (:black-secret game) (:white-secret game)))
        (is (str/includes? (:body resp) (->url :play game-id (:black-secret game))))
        (is (str/includes? (:body resp) (->url :play game-id (:white-secret game)))))))

  (testing "POST /new-game with board=beginner creates 9x9 game"
    (clear-games!)
    (let [resp (core/handler {:request-method :post
                              :uri "/new-game"
                              :params {:board "beginner"}})]
      (is (= 200 (:status resp)))
      (let [[game-id game] (first @games)]
        (is (= :beginner (:board game)))
        ;; Play page shows 9x9
        (let [play-resp (core/handler {:request-method :get
                                       :uri (->url :play game-id (:black-secret game))})]
          (is (str/includes? (:body play-resp) "Go Game (9x9)"))))))

  (testing "POST /new-game with board=intermediate creates 13x13 game"
    (clear-games!)
    (let [resp (core/handler {:request-method :post
                              :uri "/new-game"
                              :params {:board "intermediate"}})]
      (is (= 200 (:status resp)))
      (let [[game-id game] (first @games)]
        (is (= :intermediate (:board game)))
        (let [play-resp (core/handler {:request-method :get
                                       :uri (->url :play game-id (:black-secret game))})]
          (is (str/includes? (:body play-resp) "Go Game (13x13)")))))))

(deftest play-page-test
  (testing "GET /play/:game-id/:secret displays the board"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game!)
          resp-black (core/handler {:request-method :get :uri (->url :play game-id black-secret)})
          resp-white (core/handler {:request-method :get :uri (->url :play game-id white-secret)})]
      (is (= 200 (:status resp-black)))
      (is (str/includes? (:body resp-black) "You are playing as"))
      (is (str/includes? (:body resp-black) "Black"))
      (is (str/includes? (:body resp-black) "go-board"))
      (is (str/includes? (:body resp-black) "player-black"))
      (is (str/includes? (:body resp-black) "ghost-black"))
      (is (str/includes? (:body resp-black) "Your turn to move!"))

      (is (= 200 (:status resp-white)))
      (is (str/includes? (:body resp-white) "White"))
      (is (str/includes? (:body resp-white) "Waiting for opponent"))))

  (testing "Unknown game returns 404"
    (let [resp (core/handler {:request-method :get :uri (->url :play "unknown-id" "some-secret")})]
      (is (match? nil? resp))))

  (testing "Unknown route and malformed play URL return 404"
    (is (match? nil? (core/handler {:request-method :get :uri "/unknown"})))
    (is (match? nil? (core/handler {:request-method :get :uri "/play"})))
    (is (match? nil? (core/handler {:request-method :get :uri "/play/too/many/path/parts"})))
    (is (match? nil? (core/handler {:request-method :post :uri "/play/only-one"})))
    (is (match? nil? (core/handler {:request-method :post :uri "/unknown-post"})))))

(deftest gameplay-moves-test
  (testing "Black makes a move, history updates and turn switches to White"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game!)
          move-resp (core/handler {:request-method :post
                                   :uri (->url :play game-id black-secret)
                                   :params {:x "3" :y "3"}})]
      (is (match? {:status 303
                   :headers {"Location" (->url :play game-id black-secret)}}
                  move-resp))

      ;; History verified
      (is (match? [{:color :black :x 3 :y 3}]
                  (:history (get-game game-id))))

      ;; GET page for Black now shows waiting
      (let [b-resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})]
        (is (str/includes? (:body b-resp) "Waiting for opponent"))
        (is (str/includes? (:body b-resp) "stone black")))

      ;; GET page for White now shows your turn
      (let [w-resp (core/handler {:request-method :get :uri (->url :play game-id white-secret)})]
        (is (str/includes? (:body w-resp) "Your turn to move!"))
        (is (str/includes? (:body w-resp) "player-white"))
        (is (str/includes? (:body w-resp) "ghost-white"))
        (is (str/includes? (:body w-resp) "stone black")))

      ;; White moves
      (let [w-move (core/handler {:request-method :post
                                  :uri (->url :play game-id white-secret)
                                  :params {:x "15" :y "15"}})]
        (is (match? {:status 303
                     :headers {"Location" (->url :play game-id white-secret)}}
                    w-move))
        (is (= 2 (count (:history (get-game game-id))))))))

  (testing "Playing out of turn is rejected"
    (clear-games!)
    (let [{:keys [game-id white-secret]} (create-game!)
          resp (core/handler {:request-method :post
                              :uri (->url :play game-id white-secret)
                              :params {:x "3" :y "3"}})]
      (is (= 303 (:status resp)))
      (is (str/includes? (get-in resp [:headers "Location"]) "error=turn"))))

  (testing "Illegal move (occupied intersection) is rejected"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game!)]
      ;; Black plays at [3 3]
      (core/handler {:request-method :post
                     :uri (->url :play game-id black-secret)
                     :params {:x "3" :y "3"}})
      ;; White tries to play on occupied [3 3]
      (let [resp (core/handler {:request-method :post
                                :uri (->url :play game-id white-secret)
                                :params {:x "3" :y "3"}})]
        (is (= 303 (:status resp)))
        (is (str/includes? (get-in resp [:headers "Location"]) "error=illegal"))
        ;; History still only has Black's move
        (is (= 1 (count (:history (get-game game-id))))))))

  (testing "Passing turn works and consecutive passes end game"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game!)]
      ;; Black passes
      (let [resp-b (core/handler {:request-method :post
                                  :uri (->url :play game-id black-secret)
                                  :params {:action "pass"}})]
        (is (match? {:status 303} resp-b))
        (is (match? [{:color :black :pass true}]
                    (:history (get-game game-id)))))

      ;; White passes
      (let [resp-w (core/handler {:request-method :post
                                  :uri (->url :play game-id white-secret)
                                  :params {:action "pass"}})]
        (is (match? {:status 303} resp-w))
        (is (= 2 (count (:history (get-game game-id))))))

      ;; Both passes mean game over banner displayed
      (let [resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})]
        (is (str/includes? (:body resp) "game is over"))))))

(deftest board-coordinates-rendering-test
  (testing "Board renders coordinates above, below, left, and right on 9x9 game"
    (clear-games!)
    (let [{:keys [game-id black-secret]} (create-game! :beginner)
          resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})
          body (:body resp)]
      (is (= 200 (:status resp)))
      ;; Column labels A through J (skipping I) in coordinate headers
      (doseq [col ["A" "B" "C" "D" "E" "F" "G" "H" "J"]]
        (is (str/includes? body (str "board-coord-col\" scope=\"col\">" col "<"))))
      ;; Verify "I" is never rendered as a board coordinate header
      (is (not (str/includes? body "board-coord-col\" scope=\"col\">I<")))

      ;; Row labels 1 through 9 on row coordinate headers
      (doseq [row (range 1 10)]
        (is (str/includes? body (str "board-coord-row-label\" scope=\"row\">" row "<"))))

      ;; Tooltip / titles check: Bottom-left is A1!
      (is (str/includes? body "title=\"Play at A1\""))
      (is (str/includes? body "title=\"Play at A9\""))
      (is (str/includes? body "title=\"Play at J1\""))
      (is (str/includes? body "title=\"Play at J9\""))
      (is (str/includes? body "title=\"Play at E5\""))))

  (testing "Board renders coordinates on standard 19x19 game"
    (clear-games!)
    (let [{:keys [game-id black-secret]} (create-game! :standard)
          resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})
          body (:body resp)]
      (is (= 200 (:status resp)))
      ;; Standard columns go up to T
      (is (str/includes? body "board-coord-col\" scope=\"col\">A<"))
      (is (str/includes? body "board-coord-col\" scope=\"col\">T<"))
      (is (not (str/includes? body "board-coord-col\" scope=\"col\">I<")))
      ;; Top row is 19, bottom row is 1
      (is (str/includes? body "board-coord-row-label\" scope=\"row\">19<"))
      (is (str/includes? body "board-coord-row-label\" scope=\"row\">1<"))
      ;; Bottom-left is A1
      (is (str/includes? body "title=\"Play at A1\""))
      (is (str/includes? body "title=\"Play at A19\""))
      (is (str/includes? body "title=\"Play at T1\""))
      (is (str/includes? body "title=\"Play at T19\""))
      (is (str/includes? body "title=\"Play at K10\"")))))

(deftest pebble-numbering-test
  (testing "Pebbles on board show their 1-based move numbers"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game! :beginner)]
      ;; Move 1: Black plays at [4 4]
      (core/handler {:request-method :post
                     :uri (->url :play game-id black-secret)
                     :params {:x "4" :y "4"}})
      ;; Move 2: White plays at [2 2]
      (core/handler {:request-method :post
                     :uri (->url :play game-id white-secret)
                     :params {:x "2" :y "2"}})

      (let [b-resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})
            body (:body b-resp)]
        (is (= 200 (:status b-resp)))
        ;; Black pebble at [4 4] shows move number 1
        (is (str/includes? body "class=\"stone black\" role=\"img\">1</span>"))
        ;; White pebble at [2 2] shows move number 2
        (is (str/includes? body "class=\"stone white\" role=\"img\">2</span>")))))

  (testing "Direct render-board call with move-numbers displays pebble numbers"
    (let [board-spec {:board/size 9}
        board-map {[4 4] :black, [2 2] :white}
        move-nums {[4 4] 1, [2 2] 2}
        html (tmpl/render-board board-map board-spec "/play/test" :black true move-nums)]
      (is (str/includes? (hiccup/html html) "class=\"stone black\" role=\"img\">1</span>"))
      (is (str/includes? (hiccup/html html) "class=\"stone white\" role=\"img\">2</span>")))))

(deftest toggle-checkboxes-test
  (testing "Play page renders checkboxes for board and stone numbering (checked by default)"
    (clear-games!)
    (let [{:keys [game-id black-secret]} (create-game! :beginner)
          resp (core/handler {:request-method :get :uri (->url :play game-id black-secret)})
          body (:body resp)]
      (is (= 200 (:status resp)))
      (is (str/includes? body "class=\"board-controls\""))
      (is (str/includes? body "id=\"toggle-board-numbering\""))
      (is (str/includes? body "id=\"toggle-stone-numbering\""))
      (is (str/includes? body "type=\"checkbox\""))
      (is (str/includes? body "checked"))
      (is (str/includes? body "Board Numbering"))
      (is (str/includes? body "Stone Numbering"))
      (is (str/includes? body "class=\"board-container\""))))

  (testing "Server always generates numbering and coordinates regardless of query params"
    (clear-games!)
    (let [{:keys [game-id black-secret white-secret]} (create-game! :beginner)]
      ;; Make a move so stone number 1 exists
      (core/handler {:request-method :post
                     :uri (->url :play game-id black-secret)
                     :params {:x "4" :y "4"}})
      (let [resp (core/handler {:request-method :get :uri (->url :play game-id white-secret)})
            body (:body resp)]
        (is (= 200 (:status resp)))
        ;; Board coordinate headers are generated by server
        (is (str/includes? body "class=\"board-coord-col\" scope=\"col\""))
        (is (str/includes? body "class=\"board-coord-row-label\" scope=\"row\""))
        ;; Stone move number 1 is generated by server
        (is (str/includes? body "class=\"stone black\" role=\"img\">1</span>"))
        (is (str/includes? body "class=\"board-container\"")))))

  (testing "CSS stylesheet contains pure CSS rules for checkbox-based show/hide"
    (let [resp (core/handler {:request-method :get :uri "/style.css"})
          css (slurp (:body resp))]
      (is (str/includes? css "body:has(#toggle-board-numbering:not(:checked))"))
      (is (str/includes? css "body:has(#toggle-stone-numbering:not(:checked))"))
      (is (str/includes? css ".board-controls"))
      (is (str/includes? css ".toggle-control")))))

(deftest mvstore-storage-test
  (testing "storage/config holds the db-file path string directly (not a map)"
    (is (string? @config))
    (is (not (map? @config))))

  (testing "storage/store and storage/games are MVStore and MVMap defstates"
    (clear-games!)
    (is (instance? MVStore @store))
    (is (instance? MVMap @games))
    (is (instance? java.util.Map @games)))

  (testing "update-game updates existing game, passes nil on insertion, and get-game retrieves it"
    (clear-games!)
    ;; Insertion: passes nil to callback
    (let [inserted (update-game "test-1" (fn [old]
                                           (is (nil? old))
                                           {:board :intermediate :history []}))]
      (is (= {:board :intermediate :history []} inserted))
      (is (= {:board :intermediate :history []} (get-game "test-1"))))

    ;; Update: updates existing game with extra args
    (let [updated (update-game "test-1" update :history conj {:color :black :x 4 :y 4})]
      (is (= [{:color :black :x 4 :y 4}] (:history updated)))
      (is (= [{:color :black :x 4 :y 4}] (:history (get-game "test-1")))))

    ;; Update with assoc
    (let [updated (update-game "test-1" assoc :custom-field "val")]
      (is (= "val" (:custom-field updated)))
      (is (= "val" (:custom-field (get-game "test-1")))))

    ;; Subsequent move
    (update-game "test-1" update :history conj {:color :white :x 6 :y 6})
    (is (= 2 (count (:history (get-game "test-1"))))))


  (testing "MVStore file-based persistence with compression"
    (let [temp-file (java.io.File/createTempFile "gogame-mvstore" ".db")
          path (.getAbsolutePath temp-file)]
      (try
        (.delete temp-file)
        (let [store (-> (MVStore$Builder.) (.fileName path) (.compress) (.open))
              m (.openMap store "games")]
          (.put m "persisted-game" {:board :standard :history [{:color :black :x 3 :y 3}]})
          (.commit store)
          (.close store))

        ;; File exists and is non-empty
        (is (.exists (java.io.File. path)))
        (is (pos? (.length (java.io.File. path))))

        ;; Reopen and verify data is retrieved
        (let [store2 (-> (MVStore$Builder.) (.fileName path) (.compress) (.open))
              m2 (.openMap store2 "games")]
          (is (= {:board :standard :history [{:color :black :x 3 :y 3}]}
                 (get m2 "persisted-game")))
          (.close store2))
        (finally
          (.delete (java.io.File. path)))))))
