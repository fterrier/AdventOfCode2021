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
  (let [max-y (count grid)
        max-x (count (first grid))]
    (if (or (< x 0)
            (< y 0)
            (>= x max-x)
            (>= y max-y))
      9
      (get-in grid y x))))

(defn is-low-point [grid x y]
  (let [v (value grid x y)]
    (and (< v (value grid (inc x) y))
         (< v (value grid x (inc y)))
         (< v (value grid (dec x) y))
         (< v (value grid x (dec y))))))

(defn low-points [grid]
  (->> (for [x (range (count (first grid)))
             y (range (count grid))] [x y])
       (map (fn [[x y :as coord]]
              {:value (value grid x y)
               :coord coord
               :low-point (is-low-point grid x y)}))
       (filter :low-point)))

; part 1
(println (->> (parse-input "day9.txt")
              low-points
              (map :value)
              (map inc)
              (reduce +)))

(defn unvisited-basin-neighbours [grid basin [x y]]
  (let [point-value (value grid x y)
        neighbours [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]]
    (->> neighbours
         (filter
          #(let [neighbour-value (apply value grid %)]
             (and (> neighbour-value point-value)
                  (< neighbour-value 9))))
         (remove #(contains? basin %)))))

(defn find-basin [grid point]
  (loop [basin #{point}
         [point & remaining] [point]]
    (if (nil? point) basin
        (let [new-points (unvisited-basin-neighbours grid basin point)]
          (recur (apply conj basin new-points)
                 (apply conj remaining new-points))))))

; part 2
(println  (let [grid (parse-input "day9.txt")]
            (->> grid
                 low-points
                 (map :coord)
                 (map #(find-basin grid %))
                 (map count)
                 (sort #(compare %2 %1))
                 (take 3)
                 (reduce *))))