(require '[clojure.string :as str])

(defn parse-board [s]
  (let [rows (str/split s #"\n")]
    {:values (vec (map (comp vec
                             #(map (fn [x]
                                     {:val (. Integer parseInt x)
                                      :marked false}) %)
                             #(remove empty? %)
                             #(str/split % #"\s+"))
                       rows))
     :complete false}))

(defn parse-input [input]
  (let [[numbers & boards] (str/split (slurp input) #"\n\n")]
    {:numbers (map #(. Integer parseInt %) (str/split numbers #","))
     :boards (map parse-board boards)}))

(defn mark-values [values number]
  (map (fn [row]
         (map (fn [value]
                (if (= (:val value) number)
                  (assoc value :marked true)
                  value))
              row)) values))

(defn row-complete? [row]
  (empty? (remove :marked row)))

(defn is-complete? [values]
  (or (some identity (map row-complete? values))
      (some identity (map row-complete? (apply mapv vector values)))))

(defn play-board [{:keys [values complete winning-round]} number round]
  (let [new-values (mark-values values number)
        new-complete (or complete (is-complete? new-values))]
    {:values new-values
     :complete new-complete
     :winning-round (if (not= complete new-complete) round nil)}))

(defn play-bingo [{:keys [boards numbers]} round winner-fn]
  (let [boards (map #(play-board % (first numbers) round) boards)
        winner (winner-fn boards round)]
    (if winner
      {:winner winner :number (first numbers)}
      (recur {:boards boards :numbers (rest numbers)} (inc round) winner-fn))))

(defn score-board [board]
  (->> board
       (apply concat)
       (remove :marked)
       (reduce (fn [agg value] (+ agg (:val value))) 0)))

(defn first-to-win [boards _]
  (->> boards
       (filter :complete)
       first))

(defn last-to-win [boards round]
  (let [incomplete-boards (remove :complete boards)]
    (cond (= 0 (count incomplete-boards))
          (->> boards
               (filter #(= round (:winning-round %)))
               first))))

; part 1
(println
 (let [{:keys [winner number]} (play-bingo (parse-input "day4.txt") 0 first-to-win)]
   {:score (score-board (:values winner))
    :number number}))

; part 2
(println
 (let [{:keys [winner number]} (play-bingo (parse-input "day4.txt") 0 last-to-win)]
   {:score (score-board (:values winner))
    :number number}))