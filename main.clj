(require '[clojure.string :as str])

; x=20..30
; y=-10..-5

; x : 10 - 19 - 27 - 
; [6,9]= : [6,9] [11,17] [15,24] [18,30] [20,35] [21,39] [21,42] [21,44] [21,45] [21,44] [21,42] 39 35 30 24 17 9 0 -10
; [7,9]= : [7,9] [13,17] [18,24] [22,30] [25,35] [27,39] [28,42] [29,44] [29,45] [29,44] [29,42] 39 35 30 24 17 9 0 -10

; [6,10]==[_,-11]

; [0,-10]= : [0,-10]

(defn top-pos [velocity]
  (reduce + (range 0 (inc velocity))))

; part 1 - y = -93 --> velocity == 92
(println (top-pos 92))

(defn pos-x [vel]
  (map first (iterate
              (fn [[p vel]] [(+ p vel) (if (= 0 vel) 0 (dec vel))])
              [0 vel])))

(defn pos-y [vel]
  (map first (iterate
              (fn [[p vel]] [(+ p vel) (dec vel)])
              [0 vel])))

;; (println (take 10 (pos-y -4))) ; -4 -9 -15 -22 

(defn in-range [positions x-min x-max]
  (let [r (reduce
           (fn [x-1 x-2]
             (cond
               (and (>= x-1 x-min) (<= x-1 x-max)) (reduced 1)
               (= x-1 x-2) (reduced 0)
               (and (> x-2 0) (< x-1 x-min)) x-2
               (and (> x-2 0) (> x-1 x-max)) (reduced 0)
               (and (< x-2 0) (< x-1 x-min)) (reduced 0)
               (and (< x-2 0) (> x-1 x-max)) x-2))
           positions)]
    (if (= 1 r) true false)))

;; (println (take 10 (pos-y -1)))
;; (println (in-range (pos-y 0) -10 -5))

(defn candidates-x [x-min x-max]
  (concat
   (->> (range 1 x-min)
        (filter #(in-range (pos-x %) x-min x-max)))
   (range x-min (inc x-max))))

(defn candidates-y [y-min y-max]
  (concat
   (reverse (range 0 (- y-min)))
   (->> (range 0 (dec y-min) -1)
        (filter #(in-range (pos-y %) y-min y-max)))))

;; (println (candidates-x 20 30))
;; (println (candidates-y -10 -5))

(defn combinations [cand-x cand-y]
  (->> (for [x (range (count cand-x))
             y (range (count cand-y))] [x y])
       (map (fn [[x y]] [(nth cand-x x) (nth cand-y y)]))))

(defn on-target? [[x y] [min-x max-x] [min-y max-y]]
  (and (>= x min-x) (<= x max-x)
       (>= y min-y) (<= y max-y)))

(defn overshot-target? [[x y] [min-x max-x] [min-y max-y]]
  (or (> x max-x) (< y min-y)))

;; (println (on-target? [21 -6] [20 30] [-10 -5])) ; true
;; (println (overshot-target? [21 -6] [20 30] [-10 -5])) ; false
;; (println (overshot-target? [21 -11] [20 30] [-10 -5])) ; true
;; (println (overshot-target? [31 -6] [20 30] [-10 -5])) ; true

(defn hits-target [[x y] x-span y-span]
  (->> (map vector (pos-x x) (pos-y y))
       (map (fn [pos]
              (cond (on-target? pos x-span y-span)
                    true
                    (overshot-target? pos x-span y-span)
                    false
                    :else nil)))
       (remove nil?)
       first))

;; (println (hits-target [6 3] [20 30] [-10 -5]))

; part 2
(println
 (->> (combinations (candidates-x 195 238)
                    (candidates-y -93 -67))
      (filter #(hits-target % [195 238] [-93 -67]))
      count))

;; (println (count (combinations (candidates-x 195 238)
;;                               (candidates-y -93 -67))))
