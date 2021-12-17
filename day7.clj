(require '[clojure.string :as str])

(defn parse-input [input]
  (let [values (str/split (slurp input) #",")]
    (map #(. Integer parseInt %) values)))

(defn next-index [sorted-values current start-from]
  (let [value (get sorted-values start-from)]
    (if (> value current) start-from
        (recur sorted-values current (inc start-from)))))

(defn scan-distances [sorted-values sum-adjust-fn]
  (loop [sorted-values sorted-values
         sum 0
         current 0
         i 0
         acc [sum]]
    (if (>= current (last sorted-values)) acc
        (let [i (next-index sorted-values current i)
              lower (take i sorted-values)
              sum (sum-adjust-fn sum lower (inc current))]
          (recur sorted-values sum (inc current) i (conj acc sum))))))

(defn distances [values sum-adjust-fn]
  (let [dist-1 (scan-distances values
                               sum-adjust-fn)
        dist-2 (reverse (scan-distances
                         (vec (map #(- (last values) %) (reverse values)))
                         sum-adjust-fn))]
    (->> (map vector
              dist-1
              dist-2)
         (map (fn [[x y]] (+ x y))))))

; part 1
(println
 (let [values (vec (sort (parse-input "day7.txt")))
       dist (distances values (fn [sum lower _]
                                (+ sum (count lower))))]
   (apply min dist)))

; part 2
(println
 (let [values (vec (sort (parse-input "day7.txt")))
       dist (distances values (fn [sum lower current]
                                (+ sum (reduce + (map #(- current %) lower)))))]
   (apply min dist)))
