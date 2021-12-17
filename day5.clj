(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-coord [s]
  (let [[x y] (str/split s #",")]
    {:x (. Integer parseInt x) :y (. Integer parseInt y)}))

(defn parse-line [s]
  (let [[start end] (str/split s #" -> ")]
    {:start (parse-coord start) :end (parse-coord end)}))

(defn parse-input [input]
  (let [rows (str/split (slurp input) #"\n")]
    (map parse-line rows)))

(defn super-range [start end]
  (if (>= end start)
    (range start (inc end))
    (reverse (range end (inc start)))))

(defn update-pos [grid {:keys [x y]} value]
  (update grid x assoc y value))

(defn mark [grid coords]
  (reduce (fn [grid {:keys [x y] :as coord}]
            (let [value (or (-> grid
                                (get x)
                                (get y)) 0)]
              (update-pos grid coord (inc value))))
          grid coords))

(defn diag-coord [start end]
  (let [x-diff (Math/abs (- (:x start) (:x end)))
        y-diff (Math/abs (- (:y start) (:y end)))]
    (if (= x-diff y-diff)
      (->> (map vector
                (super-range (:x start) (:x end))
                (super-range (:y start) (:y end)))
           (map (fn [[x y]] {:x x :y y})))
      [])))

(defn line-coords [diag? start end]
  (cond
    (= (:x start) (:x end))
    (map (fn [y] {:x (:x start) :y y}) (super-range (:y start) (:y end)))
    (= (:y start) (:y end))
    (map (fn [x] {:x x :y (:y start)}) (super-range (:x start) (:x end)))
    diag?
    (diag-coord start end)
    :else []))

(defn mark-grid [coord-fn grid lines]
  (reduce
   (fn [grid {:keys [start end]}]
     (mark grid (coord-fn start end))) grid lines))

(defn count-overlaps [grid]
  (->> grid
       vals
       (map vals)
       (apply concat)
       (filter #(> % 1))
       count))

; part 1
(println (count-overlaps (mark-grid (partial line-coords false) {}
                                    (parse-input "day5.txt"))))

; part 2
(println (count-overlaps (mark-grid (partial line-coords true) {}
                                    (parse-input "day5.txt"))))
