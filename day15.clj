(load-file "priority-map.clj")
(require '[clojure.data.priority-map :refer [priority-map]])
(require '[clojure.string :as str])

(defn parse-input [input]
  (->> (str/split (slurp input) #"\n")
       (map (fn [line]
              (->> line
                   char-array
                   (map #(. Integer parseInt (str %)))
                   vec)))
       vec))

(defn size [grid]
  [(count grid) (count (first grid))])

(defn value [grid [x y]]
  (let [[max-x max-y] (size grid)]
    (if (or (< x 0)
            (< y 0)
            (>= x max-x)
            (>= y max-y))
      -1
      (get (get grid x) y))))

(defn lower-neighbours [[x y] risk grid lowest]
  (let [neighbours [[(dec x) y] [x (dec y)] [x (inc y)] [(inc x) y]]]
    (->> neighbours
         (map (fn [point] [point (value grid point)]))
         (remove (fn [[_ value]] (= -1 value)))
         (map (fn [[point value]] [point (+ risk value)]))
         (filter (fn [[point value]]
                   (< value (get lowest point Integer/MAX_VALUE)))))))

(defn visit-grid [grid]
  (let [[size-x size-y] (size grid)
        end [(dec size-x) (dec size-y)]]
    (loop [grid grid
           lowest {[0 0] 0}
           to-visit (priority-map [0 0] 0)]
      (let [[point risk] (peek to-visit)]
        (if (= end point)
          {:risk risk}
          (let [neighbours (lower-neighbours point risk grid lowest)]
            (recur grid
                   (into lowest neighbours)
                   (into (pop to-visit) neighbours))))))))

; part 1
;; (println (->> (parse-input "day15.txt")
;;               visit-grid))

(defn inc-value [value i]
  (let [new-value (+ value i)]
    (if (> new-value 9)
      (mod (+ value i) 9)
      new-value)))

;; (println (inc-value 9 2)) ; == 2
;; (println (inc-value 9 1)) ; == 1

(defn replicate-right [n original-grid]
  (->> {:grid original-grid :i 1}
       (iterate
        (fn [{:keys [grid i]}]
          {:grid (->>
                  (map vector grid original-grid)
                  (map (fn [[row original-row]]
                         (concat row (map #(inc-value % i) original-row)))))
           :i (inc i)}))
       (take n)
       last
       :grid))

;; (println (replicate-right 5 [[1 2] [4 5] [8 9]]))

(defn transpose [m]
  (apply mapv vector m))

(defn replicate-down [n grid]
  (->> grid
       transpose
       (replicate-right n)
       transpose))

;; (println (replicate-down 5 [[1 2] [4 5] [8 9]]))

(defn replicate-grid [n grid]
  (->> grid
       (replicate-right n)
       (replicate-down n)))

;; (println (replicate-grid 5 [[1 2] [4 5] [8 9]]))

; part 2
(println (->> (parse-input "day15.txt")
              (replicate-grid 5)
              visit-grid))