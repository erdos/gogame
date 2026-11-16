(ns erdos.gogame.board-test
  (:require [clojure.test :refer [deftest is testing]]
            [erdos.gogame.board :as b]))

(def board-spec {:board/size 19})

(deftest board-specs-test
  (testing "Standard board spec"
    (is (= 19 (get-in b/boards [:standard :board/size])))
    (is (= 9 (count (get-in b/boards [:standard :board/star-points])))))
  (testing "Intermediate board spec"
    (is (= 13 (get-in b/boards [:intermediate :board/size])))
    (is (= 5 (count (get-in b/boards [:intermediate :board/star-points])))))
  (testing "Beginner board spec"
    (is (= 9 (get-in b/boards [:beginner :board/size])))
    (is (= 5 (count (get-in b/boards [:beginner :board/star-points]))))
    ;; On beginner board, [8 8] is legal, [9 9] is out of bounds
    (is (some? (b/play-move (:beginner b/boards) {} {:color :black :x 8 :y 8})))
    (is (nil? (b/play-move (:beginner b/boards) {} {:color :black :x 9 :y 9})))))

(deftest neighbors-test
  (testing "corner has 2 neighbors"
    (is (= #{[1 0] [0 1]} (set (b/neighbors board-spec [0 0])))))
  (testing "edge has 3 neighbors"
    (is (= #{[0 0] [2 0] [1 1]} (set (b/neighbors board-spec [1 0])))))
  (testing "interior has 4 neighbors"
    (is (= #{[3 4] [5 4] [4 3] [4 5]} (set (b/neighbors board-spec [4 4]))))))

(deftest play-move-basic-test
  (testing "empty board move"
    (is (= {[3 3] :black} (b/play-move board-spec {} {:color :black :x 3 :y 3}))))

  (testing "occupied cell is illegal"
    (is (nil? (b/play-move board-spec {[3 3] :black} {:color :white :x 3 :y 3}))))

  (testing "out of bounds is illegal"
    (is (nil? (b/play-move board-spec {} {:color :black :x 19 :y 19})))
    (is (nil? (b/play-move board-spec {} {:color :black :x -1 :y 0}))))

  (testing "pass move does not alter board"
    (is (= {[3 3] :black} (b/play-move board-spec {[3 3] :black} {:color :white :pass true})))))

(deftest single-stone-capture-test
  (testing "surrounding a corner stone captures it"
    (let [play (partial b/play-move board-spec)
          board (-> {}
                    (play {:color :black :x 0 :y 0})
                    (play {:color :white :x 1 :y 0})
                    (play {:color :white :x 0 :y 1}))]
      (is (= {[1 0] :white, [0 1] :white} board))
      (is (nil? (get board [0 0]))))))

(deftest multi-stone-capture-test
  (testing "surrounding a 2-stone group captures both"
    (let [play (partial b/play-move board-spec)
          board (-> {}
                    (play {:color :black :x 1 :y 1})
                    (play {:color :black :x 1 :y 2})
                    ;; Surround with white
                    (play {:color :white :x 0 :y 1})
                    (play {:color :white :x 0 :y 2})
                    (play {:color :white :x 2 :y 1})
                    (play {:color :white :x 2 :y 2})
                    (play {:color :white :x 1 :y 0})
                    (play {:color :white :x 1 :y 3}))]
      (is (nil? (get board [1 1])))
      (is (nil? (get board [1 2])))
      (is (= 6 (count board))))))

(deftest suicide-prohibition-test
  (testing "playing into a spot with 0 liberties is suicide and prohibited"
    (let [board {[1 0] :white, [0 1] :white}]
      (is (nil? (b/play-move board-spec board {:color :black :x 0 :y 0})))))

  (testing "playing into 0 liberties is allowed if it captures opponent stones"
    ;; White stone at [0 0], Black at [1 0]. Black plays [0 1] capturing [0 0].
    (let [board {[0 0] :white, [1 0] :black}
          result (b/play-move board-spec board {:color :black :x 0 :y 1})]
      (is (= {[1 0] :black, [0 1] :black} result)))))

(deftest turn-and-history-test
  (testing "turn alternates"
    (is (= :black (b/current-turn [])))
    (is (= :white (b/current-turn [{:color :black :x 3 :y 3}])))
    (is (= :black (b/current-turn [{:color :black :x 3 :y 3}
                                  {:color :white :x 15 :y 15}]))))

  (testing "history->board generates expected board"
    (let [hist [{:color :black :x 3 :y 3}
                {:color :white :x 15 :y 15}]]
      (is (= {[3 3] :black, [15 15] :white} (b/history->board hist board-spec))))))

(deftest ko-rule-test
  (testing "simple Ko: immediate recapture is forbidden"
    (let [hist [{:color :black :x 1 :y 0}
                {:color :white :x 2 :y 0}
                {:color :black :x 0 :y 1}
                {:color :white :x 3 :y 1}
                {:color :black :x 1 :y 2}
                {:color :white :x 2 :y 2}
                {:color :black :x 2 :y 1}
                {:color :white :x 1 :y 1}]] ; White captures [2 1]
      (is (nil? (b/valid-move? board-spec hist {:color :black :x 2 :y 1}))))))

(deftest partial-fn-test
  (testing "play-move, valid-move?, and neighbors as partial functions"
    (let [pm (partial b/play-move board-spec)
          vm (partial b/valid-move? board-spec)
          nbrs (partial b/neighbors board-spec)
          b1 (pm {} {:color :black :x 0 :y 0})]
      (is (= {[0 0] :black} b1))
      (is (some? (vm [{:color :black :x 0 :y 0}] {:color :white :x 1 :y 0})))
      (is (nil? (vm [{:color :black :x 0 :y 0}] {:color :white :x 0 :y 0})))
      (is (= #{[1 0] [0 1]} (set (nbrs [0 0])))))))

(deftest coordinates-test
  (testing "Column labels skip 'I' and start with A"
    (is (= \A (b/col-label 0)))
    (is (= \B (b/col-label 1)))
    (is (= \H (b/col-label 7)))
    (is (= \J (b/col-label 8))) ; I is skipped!
    (is (= \K (b/col-label 9)))
    (is (= \T (b/col-label 18)))
    (is (not (some #{\I} b/col-label)))
    (is (= 25 (count b/col-label))))

  (testing "Row labels: bottom row is 1, top row is size"
    ;; 9x9 board: top y=0 is row 9, bottom y=8 is row 1
    (is (= "9" (b/row-label (:beginner b/boards) 0)))
    (is (= "1" (b/row-label (:beginner b/boards) 8)))
    ;; 19x19 board: top y=0 is row 19, bottom y=18 is row 1
    (is (= "19" (b/row-label (:standard b/boards) 0)))
    (is (= "1" (b/row-label (:standard b/boards) 18))))

  (testing "Bottom left is A1 across all boards"
    (is (= "A1" (b/coord->label (:beginner b/boards) [0 8])))
    (is (= "A1" (b/coord->label (:intermediate b/boards) [0 12])))
    (is (= "A1" (b/coord->label (:standard b/boards) [0 18]))))

  (testing "Corners and star points human-readable labels"
    (let [b9 (:beginner b/boards)
          b19 (:standard b/boards)]
      ;; 9x9 corners
      (is (= "A9" (b/coord->label b9 [0 0])))   ; Top-left
      (is (= "J9" (b/coord->label b9 [8 0])))   ; Top-right
      (is (= "A1" (b/coord->label b9 [0 8])))   ; Bottom-left
      (is (= "J1" (b/coord->label b9 [8 8])))   ; Bottom-right
      ;; 9x9 Tengen (center [4 4])
      (is (= "E5" (b/coord->label b9 [4 4])))
      ;; 19x19 corners
      (is (= "A19" (b/coord->label b19 [0 0])))
      (is (= "T19" (b/coord->label b19 [18 0])))
      (is (= "A1" (b/coord->label b19 [0 18])))
      (is (= "T1" (b/coord->label b19 [18 18])))
      ;; 19x19 Tengen (center [9 9])
      (is (= "K10" (b/coord->label b19 [9 9]))))))

(deftest history-move-numbers-test
  (testing "empty history has no stone numbers"
    (is (= {} (b/history->move-numbers [] board-spec))))

  (testing "moves receive 1-based sequential numbers"
    (let [hist [{:color :black :x 3 :y 3}
                {:color :white :x 15 :y 15}]]
      (is (= {[3 3] 1, [15 15] 2} (b/history->move-numbers hist board-spec)))))

  (testing "passes do not place a stone, subsequent moves keep 1-based move index"
    (let [hist [{:color :black :x 3 :y 3}
                {:color :white :pass true}
                {:color :black :x 4 :y 4}]]
      (is (= {[3 3] 1, [4 4] 3} (b/history->move-numbers hist board-spec)))))

  (testing "captured stones are removed from move numbers"
    (let [hist [{:color :black :x 0 :y 0}
                {:color :white :x 1 :y 0}
                {:color :black :x 5 :y 5}
                {:color :white :x 0 :y 1} ; captures [0 0]
                {:color :black :x 6 :y 6}]]
      (is (= {[1 0] 2, [5 5] 3, [0 1] 4, [6 6] 5}
             (b/history->move-numbers hist board-spec))))))
