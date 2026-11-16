(ns erdos.gogame.board)

(def boards
  {:standard     {:board/size 19 :board/star-points #{[3 3] [9 3] [15 3] [3 9] [9 9] [15 9] [3 15] [9 15] [15 15]}}
   :intermediate {:board/size 13 :board/star-points #{[3 3] [9 3] [6 6] [3 9] [9 9]}}
   :beginner     {:board/size 9  :board/star-points #{[2 2] [6 2] [4 4] [2 6] [6 6]}}})

; Skip the I!
(def col-label (vec "ABCDEFGHJKLMNOPQRSTUVWXYZ"))

(defn row-label [board-spec y]
  (str (- (:board/size board-spec) y)))

(defn coord->label [board-spec [x y]]
  (str (col-label x) (row-label board-spec y)))

(def opponent {:black :white, :white :black})

(defn pos-legal? [{:board/keys [size]} [x y]]
  (and (<= 0 x (dec size)) (<= 0 y (dec size))))

(defn neighbors [board-spec [x y]]
  (filter (partial pos-legal? board-spec)
          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

; board = {[x y] :color}
(defn find-group [board start board-spec]
  (let [color (get board start)]
    (loop [queue [start]
           visited #{start}
           group #{start}]
      (if (empty? queue)
        group
        (let [curr (peek queue)
              unvisited (->> (neighbors board-spec curr)
                             (into []
                                   (comp
                                     (filter #(= (get board %) color))
                                     (remove visited))))]
          (recur (into (pop queue) unvisited)
                 (into visited unvisited)
                 (into group unvisited)))))))

(defn group-liberties [board group board-spec]
  (->> group
       (mapcat (partial neighbors board-spec))
       (remove board)
       set))

(defn- group-calculed [board-spec board pos opp]
  (->> (neighbors board-spec pos)
       (filter #(= (get board %) opp))
       (reduce
         (fn [acc nbr]
           (if (acc nbr)
             acc
             (let [g (find-group board nbr board-spec)]
               (if (empty? (group-liberties board g board-spec))
                 (into acc g)
                 acc))))
         #{})))

(defn play-move [board-spec board {:keys [color x y pass]}]
  (if pass
    board
    (let [pos [x y]]
      (when (and (pos-legal? board-spec pos)
                 (nil? (get board pos)))
        (let [opp (opponent color)
              board (assoc board pos color)
              board (reduce dissoc board (group-calculed board-spec board pos opp))
              my-grp (find-group board pos board-spec)]
          (when (seq (group-liberties board my-grp board-spec))
            ; No suicide: new group must have some liberty
            board))))))

(defn history->board [history board-spec]
  (reduce (partial play-move board-spec) {} history))

(defn history->move-numbers [history board-spec]
  (let [board-spec (if (map? board-spec) board-spec (get boards board-spec (:standard boards)))]
    (loop [moves (map-indexed vector history)
           board {}
           stone-moves {}]
      (if (empty? moves)
        stone-moves
        (let [[idx {:keys [pass] :as move}] (first moves)]
          (if pass
            (recur (rest moves) board stone-moves)
            (if-let [new-board (play-move board-spec board move)]
              (let [pos [(:x move) (:y move)]
                    surviving-moves (select-keys stone-moves (keys new-board))]
                (recur (rest moves)
                       new-board
                       (assoc surviving-moves pos (inc idx))))
              (recur (rest moves) board stone-moves))))))))

(defn current-turn [history]
  (-> history count (mod 2) [:black :white]))

(defn valid-move? [board-spec history {:keys [pass] :as move}]
  (if pass
    (history->board history board-spec)
    (when-let [new-board (play-move board-spec (history->board history board-spec) move)]
      ;; Ko rule: cannot recreate board from 2 half-moves ago
      (when-not (and (> (count history) 1)
                     (= new-board (history->board (pop history) board-spec)))
        new-board))))

(defn captured-count [history board color]
  (let [played (->> history
                    (filter #(and (= (:color %) color) (not (:pass %))))
                    count)
        on-board (->> (vals board)
                      (filter #{color})
                      count)]
    (max 0 (- played on-board))))
