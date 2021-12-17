(require '[clojure.string :as str])

(defn parse-code [s]
  (->> (str/split s #" ")
       (map (comp vec sort))
       (map #(apply str %))
       vec))

(defn parse-input [input]
  (->> (str/split (slurp input) #"\n")
       (map (fn [line] (vec (str/split line #" \| "))))
       (map (fn [[patterns output]] {:patterns (parse-code patterns)
                                     :output (parse-code output)}))))

; part 1
(println (->> (parse-input "day8.txt")
              (mapcat :output)
              (filter #(get #{2 3 4 7} (count %)))
              (count)))

(defn union [s1 s2]
  (->> #{}
       (#(apply conj % s1))
       (#(apply conj % s2))
       vec sort (apply str)))

(defn diff [s1 s2]
  (->> s1
       (remove #(.contains s2 (str %)))
       (apply str)))

(defn get-mapping [patterns]
  (let [one (->> patterns (filter #(= 2 (count %))) first)
        four (->> patterns (filter #(= 4 (count %))) first)
        seven (->> patterns (filter #(= 3 (count %))) first)
        eight (->> patterns (filter #(= 7 (count %))) first)
        two (->> patterns (filter #(and (= 5 (count %))
                                        (= 2 (count (diff % (union four seven)))))) first)
        nine (->> patterns (filter #(and (= 6 (count %))
                                         (= 1 (count (diff % (union four seven)))))) first)
        three (->> patterns (filter #(and (= 5 (count %))
                                          (= 3 (count (diff % one))))) first)
        five (->> patterns (filter #(and (= 5 (count %))
                                         (not= three %)
                                         (not= two %))) first)
        zero (->> patterns (filter #(and (= 6 (count %))
                                         (= 2 (count (diff % five))))) first)
        six (->> patterns (filter #(and (= 6 (count %))
                                        (not= zero %)
                                        (not= nine %))) first)]
    (-> {}
        (assoc zero 0)
        (assoc one 1)
        (assoc two 2)
        (assoc three 3)
        (assoc four 4)
        (assoc five 5)
        (assoc six 6)
        (assoc seven 7)
        (assoc eight 8)
        (assoc nine 9))))

(defn decode [output mapping]
  (->> output
       (map #(get mapping %))
       (apply str)
       (. Integer parseInt)))

(println (->> (parse-input "day8.txt")
              (map #(assoc % :mapping (get-mapping (:patterns %))))
              (map #(decode (:output %) (:mapping %)))
              (reduce +)))

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab

;; 1 == len(2)
;; 4 == len(4)
;; 7 == len(3)
;; 8 == len(7)