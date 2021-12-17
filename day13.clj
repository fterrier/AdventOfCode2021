(require '[clojure.string :as str])

(defn parse-input [input]
  (let [[points instructions] (str/split (slurp input) #"\n\n")]
    {:points
     (->> (str/split points #"\n")
          (map (fn [point]
                 (into [] (map #(. Integer parseInt %)
                               (str/split point #",")))))
          set)
     :instructions
     (->> (str/split instructions #"\n")
          (map (fn [instruction]
                 (let [[words fold] (str/split instruction #"=")]
                   {:axis (last words)
                    :fold (. Integer parseInt fold)})))
          vec)}))

(defn invert [y size-y]
  (- size-y y 1))

;; (println (invert 0 7)) ; == 6
;; (println (invert 1 7)) ; == 5
;; (println (invert 5 7)) ; == 1
;; (println (invert 6 7)) ; == 0

(defn new-coord [c fold size]
  (if (= c fold) -1
      (if (>= fold (/ (dec size) 2))
        (if (< c fold)
          ; the bottom part stays at the same spot
          c
          (+ (invert (- c fold) (- size fold))
             (- (inc size) (* 2 (- size fold)))))
        (if (< c fold)
          ; the bottom part moves up 
          (+ c (- (* 2 (- size fold)) (inc size)))
          (invert (- c fold) (- size fold))))))

;; (println (new-coord 14 6 15)) ; == 0
;; (println (new-coord 13 6 15)) ; == 1
;; (println (new-coord 12 6 15)) ; == 2
;; (println (new-coord 11 6 15)) ; == 3
;; (println (new-coord 10 6 15)) ; == 4
;; (println (new-coord 9 6 15)) ; == 5
;; (println (new-coord 8 6 15)) ; == 6
;; (println (new-coord 7 6 15)) ; == 7
;; (println (new-coord 6 6 15)) ; == -1
;; (println (new-coord 5 6 15)) ; == 7
;; (println (new-coord 4 6 15)) ; == 6
;; (println (new-coord 3 6 15)) ; == 5
;; (println (new-coord 2 6 15)) ; == 4
;; (println (new-coord 1 6 15)) ; == 3
;; (println (new-coord 0 6 15)) ; == 2
;; (println "bottom")
;; (println (new-coord 0 7 15)) ; == 0
;; (println (new-coord 1 7 15)) ; == 1
;; (println (new-coord 2 7 15)) ; == 2
;; (println (new-coord 3 7 15)) ; == 3
;; (println (new-coord 4 7 15)) ; == 4
;; (println (new-coord 5 7 15)) ; == 5
;; (println (new-coord 6 7 15)) ; == 6
;; (println (new-coord 7 7 15)) ; == -1
;; (println (new-coord 8 7 15)) ; == 6
;; (println (new-coord 9 7 15)) ; == 5
;; (println (new-coord 10 7 15)) ; == 4
;; (println (new-coord 11 7 15)) ; == 3
;; (println (new-coord 12 7 15)) ; == 2
;; (println (new-coord 13 7 15)) ; == 1
;; (println (new-coord 14 7 15)) ; == 0

(defn new-size [size fold]
  (max fold (dec (- size fold))))

;; (println (new-size 15 7)) ; == 7
;; (println (new-size 15 8)) ; == 8
;; (println (new-size 15 6)) ; == 8
;; (println (new-size 15 5)) ; == 9
;; (println (new-size 15 4)) ; == 10
;; (println (new-size 15 3)) ; == 11
;; (println (new-size 15 2)) ; == 12
;; (println (new-size 15 1)) ; == 13
;; (println (new-size 15 0)) ; == 14

(defn fold-horizontal [{:keys [points size]} fold-y]
  {:points (->> points
                (map (fn [[x y]]
                       [x (new-coord y fold-y (second size))]))
                set)
   :size (assoc size 1 (new-size (second size) fold-y))})

(defn fold-vertical [{:keys [points size]} fold-x]
  {:points (->> points
                (map (fn [[x y]]
                       [(new-coord x fold-x (first size)) y]))
                set)
   :size (assoc size 0 (new-size (first size) fold-x))})

(defn fold [points-with-size {:keys [axis fold]}]
  (cond (= axis \x)
        (fold-vertical points-with-size fold)
        (= axis \y)
        (fold-horizontal points-with-size fold)))

; part 1
(println (let [{:keys [points instructions]} (parse-input "day13.txt")
               size [(->> points (map first) (apply max) inc)
                     (->> points (map second) (apply max) inc)]]
           (->> (fold {:points points :size size} (first instructions))
                :points
                count)))

(defn make-grid [c x y]
  (->> (repeat x c)
       (into [])
       (repeat y)
       (into [])))

(defn display [{:keys [points size]}]
  (let [[size-x size-y]  size
        grid (make-grid \. size-x size-y)]
    (->> (reduce (fn [grid [x y]]
                   (assoc-in grid [y x] \#)) grid points)
         (map #(apply str %))
         (str/join "\n"))))

; part 2
(println (let [{:keys [points instructions]} (parse-input "day13.txt")
               size [(->> points (map first) (apply max) inc)
                     (->> points (map second) (apply max) inc)]]
           (->> (reduce (fn [points-with-size instruction]
                          (fold points-with-size instruction)) 
                          {:points points :size size} instructions)
                (display))))