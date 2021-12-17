(require '[clojure.string :as str])

;; AA -> ABA             -> ABBBA
;;                          BABBB

;; AA -> B {B: +1 AB: +1 BA: +1 AA: -1} 
;; {A: 2 B: 1 AB: 1 BA: 1} -> {A: 2 B: 3 AB: 1 BB: 2 BA: 1} -> 

;; AB -> B {B: +1 AB: +1 BB: +1 AB: -1}
;; {A: 1 B: 2 AB: 1 BB: 1} -> 

;; BA -> B {B: +1 BB: +1 BA: +1 BA: -1}
;; {B: 2 A: 1 BB: 1 BA: 1} ->

;; BB -> A {B: +1 BA: +1 AB: +1 BB: -1}
;; {B: 2 A: 1 BA: 1 AB: 1} ->

(defn deltas [pair element]
  (let [[f s] [(subs pair 0 1) (subs pair 1 2)]]
    {:elements {element 1}
     :pairs (update {(str f element) 1 (str element s) 1}
                    pair (fn [v] (if (nil? v) -1 (dec v))))}))

(defn parse-input [input]
  (let [[template rules] (str/split (slurp input) #"\n\n")]
    {:template template
     :deltas
     (->> (str/split rules #"\n")
          (map (fn [rule]
                 (let [[pair element] (str/split rule #" -> ")]
                   [pair (deltas pair element)])))
          (into {}))}))

(defn freq-map [s]
  {:pairs (->> (map (fn [i] [(subs s i (+ i 2)) 1])
                    (range 0 (dec (count s))))
               (into {}))
   :elements (->> (char-array s)
                  (map str)
                  frequencies
                  (into {}))})

(defn add-stuff [m stuff deltas mult]
  (reduce (fn [m [c value]]
            (update-in m [stuff c] (fn [v] (if (nil? v) (* mult value) (+ v (* mult value)))))) m (get deltas stuff)))

(defn apply-deltas [m deltas]
  (reduce (fn [m [pair mult]]
            (-> m
                (add-stuff :elements (get deltas pair) mult)
                (add-stuff :pairs (get deltas pair) mult)))
          m (:pairs m)))

; part 1
(println (let [{:keys [template deltas]} (parse-input "day14.txt")]
           (->> (freq-map template)
                (iterate (fn [m]
                           (apply-deltas m deltas)))
                (take 11)
                last
                :elements
                vals
                sort)))

; part 2
(println (let [{:keys [template deltas]} (parse-input "day14.txt")]
           (->> (freq-map template)
                (iterate (fn [m]
                           (apply-deltas m deltas)))
                (take 41)
                last
                :elements
                vals
                sort)))