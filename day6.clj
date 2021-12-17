(require '[clojure.string :as str])

(defn parse-input [input]
  (let [values (str/split (slurp input) #",")]
    (reduce (fn [m value]
              (update m value #(if (nil? %) 1 (inc %))))
            {} (map #(. Integer parseInt %) values))))

(defn decrease [timers]
  (reduce-kv (fn [timers timer amount]
               (case timer
                 0 (-> timers
                       (assoc 8 amount)
                       (update 6 #(if (nil? %) amount (+ amount %))))
                 (update timers (dec timer) #(if (nil? %) amount (+ amount %)))))
             {} timers))

(defn count-all [timers]
  (->> timers
       vals
       (reduce +)))

; part 1
(println (count-all (last (take 81 (iterate decrease (parse-input "day6.txt"))))))

; part 2
(println (count-all (last (take 257 (iterate decrease (parse-input "day6.txt"))))))