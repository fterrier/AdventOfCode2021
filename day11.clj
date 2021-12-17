(load-file "priority-map.clj")
(require '[clojure.data.priority-map :refer [priority-map-by]])
(require '[clojure.string :as str])

(defn parse-input [input]
  (->> (str/split (slurp input) #"\n")
       (map (fn [line]
              (->> line
                   char-array
                   (map #(. Integer parseInt (str %)))
                   vec)))
       vec))

(defn value [grid x y]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (if (or (< x 0)
            (< y 0)
            (>= x max-x)
            (>= y max-y))
      0
      (get (get grid y) x))))

(defn priority-flash [grid]
  (let [p (priority-map-by >)]
    (->> (for [x (range (count (first grid)))
               y (range (count grid))] [x y])
         (map (fn [[x y :as point]]
                [point (value grid x y)]))
         (into p))))

(defn inc-neighbours [[x y] p]
  (let [neighbours [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                    [x (dec y)] [x (inc y)]
                    [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]]]
    (->> neighbours
         (map (fn [point] [point (get p point)]))
         (remove (fn [[_ value]] (nil? value)))
         (map (fn [[point value]] [point (inc value)])))))

(defn update-grid [grid flashers p]
  (let [new-values (into p (map (fn [point] [point 0]) flashers))]
    (reduce
     (fn [grid [[x y] value]] (assoc grid y (assoc (get grid y) x value)))
     grid new-values)))

(defn size [grid]
  (* (count grid) (count (first grid))))

(defn flash-grid [grid]
  (loop [grid grid
         flashed #{}
         to-flash (priority-flash grid)]
    (let [[point value :as f] (peek to-flash)]
      (if (or (nil? f) (<= value 9))
        {:grid (update-grid grid flashed to-flash)
         :flashers (count flashed)
         :all-flashed (= (count flashed) (size grid))}
        (let [r (pop to-flash)]
          (recur grid
                 (conj flashed point)
                 (into r (inc-neighbours point r))))))))

(defn inc-grid [grid]
  (->> grid
       (map #(map inc %))
       (map vec)
       vec))

(defn make-step [{:keys [grid flashers steps]}]
  (let [result
        (->> grid
             inc-grid
             flash-grid)]
    {:grid (:grid result)
     :flashers (+ flashers (:flashers result))
     :all-flashed (:all-flashed result)
     :steps (inc steps)}))

; part 1
(println (->> {:grid (parse-input "day11.txt") :flashers 0 :steps 0}
              (iterate make-step)
              (take 101)
              (last)
              :flashers
              ))

; part 2
(println (->> {:grid (parse-input "day11.txt") :flashers 0 :steps 0}
              (iterate make-step)
              (filter :all-flashed)
              (first)
              :steps))