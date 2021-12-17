(require '[clojure.string :as str])

(defn parse-input [input]
  (str/split (slurp input) #"\n"))

(defn first-illegal [[f & r] stack]
  (let [[begin-stack [l]] (split-at (dec (count stack)) stack)]
    (cond (nil? f) {:stack stack}
          (contains? #{\[ \( \< \{} f)
          (first-illegal r (conj stack f))
          (or (and (= \] f) (= \[ l))
              (and (= \> f) (= \< l))
              (and (= \} f) (= \{ l))
              (and (= \) f) (= \( l)))
          (recur r (vec begin-stack))
          :else {:first-illegal f :stack stack})))

(defn score-illegal [c]
  (cond (= c \]) 57
        (= c \)) 3
        (= c \>) 25137
        (= c \}) 1197
        :else 0))

; part 1
(println (->> (parse-input "day10.txt")
              (map #(first-illegal % []))
              (map :first-illegal)
              (map score-illegal)
              (reduce +)))

(defn complete-chunk [stack s]
  (let [[begin-stack [c]] (split-at (dec (count stack)) stack)]
    (if (nil? c) s
        (recur begin-stack
               (conj s
                     (cond (= c \[) \]
                           (= c \() \)
                           (= c \<) \>
                           (= c \{) \}))))))

(defn score-pad [ch]
  (reduce (fn [score c]
            (+ (cond (= c \]) 2
                     (= c \)) 1
                     (= c \>) 4
                     (= c \}) 3) (* score 5))) 0 ch))

; part 2
(println (->> (parse-input "day10.txt")
              (map #(first-illegal % []))
              (filter #(nil? (:first-illegal %)))
              (map :stack)
              (map #(complete-chunk % []))
              (map score-pad)
              sort
              (#(nth % (/ (dec (count %)) 2)))))