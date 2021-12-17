(defn count-larger [measurements]
  (:larger
   (reduce (fn [agg measurement]
             {:last measurement :larger (+ (:larger agg)
                                           (if (> measurement (:last agg)) 1 0))})
           {:last Integer/MAX_VALUE :larger 0} measurements)))

(defn as-vec [input]
  (with-open [rdr (clojure.java.io/reader input)]
    (vec (map #(. Integer parseInt %) (line-seq rdr)))))

(defn sliding-window [measurements n]
  (let [{:keys [window result]}
        (reduce
         (fn [agg measurement]
           {:window (conj (vec (drop 1 (:window agg))) measurement)
            :result (conj (:result agg) (:window agg))})
         {:window (vec (take n measurements)) :result []}
         (drop n measurements))]
    (conj result window)))

; part 1
(println (count-larger (as-vec "measurements.txt")))

; part 2
(println
 (count-larger
  (map (partial reduce +)
       (sliding-window (as-vec "measurements.txt") 3))))
