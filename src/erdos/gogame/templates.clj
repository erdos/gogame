(ns erdos.gogame.templates
  (:require [clojure.string :as str]
            [erdos.gogame.board :as board]
            [hiccup.page :refer [html5]]
            [ring.util.codec :as codec]))

(defn ->url [& args]
  (let [to-str #(if (ident? %) (name %) (str %))
        [req frags] (cond
                      (nil? (first args)) [nil (rest args)]
                      (map? (first args)) [(first args) (rest args)]
                      :else               [nil args])
        last-map? (map? (last frags))
        path-frags (if last-map? (butlast frags) frags)
        query-map (when last-map? (last frags))
        base (when req
               (when-let [host (get-in req [:headers "host"])]
                 (str (-> req :scheme (or :http) name) "://" host)))
        path (str "/" (->> path-frags
                           (map #(-> % to-str (str/replace #"^/+|/+$" "")))
                           (remove str/blank?)
                           (str/join "/")))
        query (when (seq query-map)
                (str "?" (->> query-map
                              (map (fn [[k v]]
                                     (str (codec/url-encode (to-str k))
                                          "="
                                          (codec/url-encode (to-str v)))))
                              (str/join "&"))))]
    (str base path query)))

(defn not-found-page []
  (html5 [:head
          [:title "404 Not Found"]
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:link {:rel "stylesheet" :href "/style.css"}]]
         [:body
          [:div.container
           [:h1 "404 - Not Found"]
           [:p "The requested page or game does not exist."]
           [:div {:style "margin-top: 16px;"}
            [:a.btn {:href "/"} "Back to Home"]]]]))

(defn welcome-page []
  (html5
    {:lang "en"}
    [:head
     [:title "Go Board Game"]
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body
     [:div.container
      [:h1 "Go Board Game"]
      [:p "Welcome to Go! Choose a board size and start a game for two players."]
      [:form {:method "POST" :action "/new-game" :style "margin-top: 16px;"}
       [:div {:style "margin-bottom: 16px;"}
        [:label {:for "board-size-select" :style "font-weight: 600; margin-right: 8px;"} "Board Size:"]
        [:select#board-size-select {:name "board"}
         [:option {:value "standard" :selected true} "Standard (19x19)"]
         [:option {:value "intermediate"} "Intermediate (13x13)"]
         [:option {:value "beginner"} "Beginner (9x9)"]]]
       [:button.btn {:type "submit"} "New Game"]]]]))

(defn game-created-page [{:keys [board-name board-spec black-url white-url]}]
  (html5
    {:lang "en"}
    [:head
     [:title "New Game Created - Go Board Game"]
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body
     [:div.container.container-wide
      [:h1 "Game Created!"]
      [:p "Size: " [:strong (str board-name " (" (:board/size board-spec) "x" (:board/size board-spec) ")")]]
      [:p "Share each secret link below with the respective player. Black plays first."]
      [:div.two-columns
       [:div.column
        [:h3 [:span.badge.badge-black "Black Player"]]
        [:div.url-box [:a {:href black-url} black-url]]]
       [:div.column
        [:h3 [:span.badge.badge-white "White Player"]]
        [:div.url-box [:a {:href white-url} white-url]]]]
      [:div {:style "margin-top: 20px;"}
       [:a.btn {:href "/"} "Back to Home"]]]]))

(defn render-board
  ([board-map board-spec play-url color interactive?]
   (render-board board-map board-spec play-url color interactive? nil))
  ([board-map board-spec arg3 arg4 arg5 arg6]
   (if (boolean? arg5)
     (let [play-url arg3
           color arg4
           interactive? arg5
           move-numbers arg6
           board-spec (if (map? board-spec) board-spec (get board/boards board-spec (:standard board/boards)))
           stars (or (:board/star-points board-spec) #{})
           size (:board/size board-spec)
           board-classes ["go-board" (when (and interactive? color) (str "player-" (name color)))]
           ghost-class (when (and interactive? color) (str "ghost-" (name color)))]
       [:table {:class board-classes}
        [:thead
         [:tr.board-coord-row
          [:th.board-coord-corner]
          (for [x (range size)]
            [:th.board-coord-col (board/col-label x)])
          [:th.board-coord-corner]]]
        [:tbody
         (for [y (range size)]
           (let [r-label (board/row-label board-spec y)]
             [:tr {:key (str "row-" y)}
              [:th.board-coord-row-label r-label]
              (for [x (range size)]
                (let [pos [x y]
                      stone (get board-map pos)
                      color (cond
                              (keyword? stone) stone
                              (map? stone)     (:color stone)
                              (vector? stone)  (first stone))
                      num   (cond
                              (and move-numbers (contains? move-numbers pos)) (get move-numbers pos)
                              (map? stone)    (or (:number stone) (:num stone) (:move stone))
                              (vector? stone) (second stone))
                      is-star? (contains? stars pos)
                      cell-classes ["go-cell"
                                    (when (zero? x) "left-edge")
                                    (when (= x (dec size)) "right-edge")
                                    (when (zero? y) "top-edge")
                                    (when (= y (dec size)) "bottom-edge")]]
                  [:td {:key (str "cell-" x "-" y) :class cell-classes}
                   (when (and is-star? (nil? stone))
                     [:div.star-point])
                   (cond
                     (= color :black) [:div.stone.black (when num (str num))]
                     (= color :white) [:div.stone.white (when num (str num))]
                     interactive?
                     [:form.cell-form {:method "POST" :action play-url}
                      [:input {:type "hidden" :name "x" :value (str x)}]
                      [:input {:type "hidden" :name "y" :value (str y)}]
                      [:button {:class ["cell-btn" ghost-class]
                                :type "submit"
                                :title (str "Play at " (board/coord->label board-spec pos))}]])]))
              [:th.board-coord-row-label r-label]]))]
        [:tfoot
         [:tr.board-coord-row
          [:th.board-coord-corner]
          (for [x (range size)]
            [:th.board-coord-col (board/col-label x)])
          [:th.board-coord-corner]]]])
     ;; Called as: [board-map board-spec game-id secret color interactive?]
     (render-board board-map board-spec (->url :play arg3 arg4) arg5 arg6 nil)))
  ([board-map board-spec game-id secret color interactive? move-numbers]
   (render-board board-map board-spec (->url :play game-id secret) color interactive? move-numbers)))

(defn play-page [{:keys [game-id secret color board-spec current-board move-numbers history
                         my-turn? game-over? black-captured white-captured error play-url]}]
  (let [play-url (or play-url (->url :play game-id secret))
        move-nums (or move-numbers
                      (when history
                        (board/history->move-numbers history board-spec)))]
    (html5
    {:lang "en"}
    [:head
     [:title (str "Go (" (str/capitalize (name color)) ") - Game " game-id)]
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     (when (and (not my-turn?) (not game-over?))
       [:script "setTimeout(function(){ location.replace(window.location.href); }, 4000);"])
     (when (and (not my-turn?) (not game-over?))
       [:noscript [:meta {:http-equiv "refresh" :content "4"}]])
     [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body
     [:script
      "history.replaceState(null, '', window.location.href);
       document.addEventListener('submit', function(e) {
         if (e.target && e.target.tagName === 'FORM') {
           e.preventDefault();
           var url = e.target.getAttribute('action') || window.location.href;
           fetch(url, {
             method: 'POST',
             body: new URLSearchParams(new FormData(e.target))
           }).then(function(res) {
             location.replace(res.url || window.location.href);
           }).catch(function() {
             HTMLFormElement.prototype.submit.call(e.target);
           });
         }
       });"]
     [:div.status-bar
      [:h2 (str "Go Game (" (:board/size board-spec) "x" (:board/size board-spec) ")")]
      [:p
       "You are playing as "
       [:span {:class (if (= color :black) "badge badge-black" "badge badge-white")}
        (str/capitalize (name color))]]
      [:div.captures
       [:span "Captured by Black: " [:strong (str black-captured)]]
       [:span {:style "margin: 0 12px;"} "|"]
       [:span "Captured by White: " [:strong (str white-captured)]]]
      (when game-over?
        [:div.alert-info "Both players passed consecutively. The game is over!"])
      (when error
        [:div.alert-error
         (case error
           "illegal" "Illegal move! The intersection is occupied, suicidal, or violates the Ko rule."
           "turn" "Please wait for your turn!"
           "Invalid move request.")])
      (when-not game-over?
        [:p
         (if my-turn?
           [:strong {:style "color: #15803d; font-size: 1.1em;"} "Your turn to move!"]
           [:span {:style "color: #6b7280;"} "Waiting for opponent's move... (auto-refreshing)"])])]
     [:div.board-controls
      [:label.toggle-control
       [:input#toggle-board-numbering {:type "checkbox" :checked true}]
       " Board Numbering"]
      [:label.toggle-control
       [:input#toggle-stone-numbering {:type "checkbox" :checked true}]
       " Stone Numbering"]]
     [:div.board-container
      (render-board current-board board-spec play-url color (and my-turn? (not game-over?)) move-nums)]
     (when (and my-turn? (not game-over?))
       [:div {:style "margin-top: 20px; text-align: center;"}
        [:form {:method "POST" :action play-url :style "display: inline;"}
         [:input {:type "hidden" :name "action" :value "pass"}]
         [:button.btn.btn-secondary {:type "submit"} "Pass Turn"]]])
     [:script
      "['toggle-board-numbering', 'toggle-stone-numbering'].forEach(id => {
            const cb = document.getElementById(id);
            if (!cb) return;
            try { const v = localStorage.getItem(id); if (v !== null) cb.checked = v === 'true'; } catch {}
            cb.addEventListener('change', () => { try { localStorage.setItem(id, cb.checked); } catch {} });
         });"]])))
