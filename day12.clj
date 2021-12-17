(require '[clojure.string :as str])

(defn add-path [graph start end]
  (update graph start
          (fn [value]
            (if (nil? value) [end] (conj value end)))))

(defn parse-input [input]
  (->> (str/split (slurp input) #"\n")
       (map (fn [line] (str/split line #"-")))
       (#(reduce (fn [graph [start end]]
                   (-> graph
                       (add-path start end)
                       (add-path end start))) {} %))))

(defn small-cave? [c]
  (= (str/lower-case c) c))

(defn traverse [graph {:keys [path small-caves twice] :as s} start n twice-allowed]
  (cond (= start "end")
        ;; [{;;:path (conj path start)
        ;;   :small-caves small-caves}]
        1
        (and (not n) (= start "start")) 0
        (and (contains? small-caves start)
             (if twice-allowed twice true)) 0
        :else
        (reduce +
               (map (fn [neigh]
                      (traverse
                       graph
                       {;;:path (conj path start)
                        :small-caves
                        (if (small-cave? start)
                          (conj small-caves start) small-caves)
                        :twice (or twice (if (small-cave? start)
                                           (contains? small-caves start) false))}
                       neigh
                       false
                       twice-allowed))
                    (get graph start)))))

; part 1
(println (-> (parse-input "day12.txt")
             (traverse {:path [] :small-caves #{}} "start" true false)))

; part 2
(println (-> (parse-input "day12.txt")
             (traverse {:path [] :small-caves #{}} "start" true true)))
