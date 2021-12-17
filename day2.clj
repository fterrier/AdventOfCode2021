(require '[clojure.string :as str])

(defn parse-command [command-str]
  (let [[word value] (str/split command-str #" ")]
    [(keyword word) (. Integer parseInt value)]))

(defn as-position-vec [input]
  (with-open [rdr (clojure.java.io/reader input)]
    (vec (map parse-command (line-seq rdr)))))

(defn new-position [{:keys [horizontal depth]} [word value]]
  (case word
    :up {:horizontal horizontal :depth (- depth value)}
    :down {:horizontal horizontal :depth (+ depth value)}
    :forward {:horizontal (+ horizontal value) :depth depth}))

(defn follow-instructions
  [position-fn pos [instruction & remaining-instructions :as instructions]]
  (if (empty? instructions) pos
      (recur position-fn (position-fn pos instruction) remaining-instructions)))

; part 1
(println
 (follow-instructions new-position {:horizontal 0 :depth 0} (as-position-vec "day2.txt")))

(defn new-position-with-aim
  [{:keys [horizontal depth aim] :as pos} [word value]]
  (case word
    :up (assoc pos :aim (- aim value))
    :down (assoc pos :aim (+ aim value))
    :forward (assoc pos
                    :horizontal (+ horizontal value)
                    :depth (+ depth (* value aim)))))

; part 2
(println
 (follow-instructions new-position-with-aim {:horizontal 0 :depth 0 :aim 0} (as-position-vec "day2.txt")))
