(require '[clojure.string :as str])

(defn parse-row [row]
  (->> row
       char-array
       (map (fn [c] (if (= c \#) 1 0)))
       (apply str)))

(defn parse-input [input]
  (let [[algorithm image] (str/split (slurp input) #"\n\n")]
    {:algorithm (parse-row algorithm)
     :image (vec (map parse-row (str/split image #"\n")))}))

(defn pad-image [image pad pad-char]
  (let [size-x (count (first image))
        pad-line (repeat pad (apply str (repeat (+ (* 2 pad) size-x) pad-char)))
        pad-zeros (apply str (repeat pad pad-char))]
    (->> image
         (map #(str pad-zeros % pad-zeros))
         (#(concat pad-line % pad-line))
         vec)))

(defn unpad-image [image pad]
  (map (fn [row] (subs row pad (- (count row) pad)))
       (subvec image pad (- (count image) pad))))

(println (pad-image ["111" "111" "111"] 3 \0))
(println (unpad-image (pad-image ["111" "111" "111"] 1 \0) 1))

(defn pixel-square [image [x y]]
  (->> [(dec y) y (inc y)]
       (map #(subs (get image %) (dec x) (+ 3 (dec x))))
       (apply str)))

;; (println (pixel-square ["123" "456" "789"] [1 1]))

(defn to-int [bits]
  (. Integer parseInt bits 2))

(defn apply-algorithm [algorithm pixel-square]
  (let [index (to-int pixel-square)]
    (subs algorithm index (inc index))))

(defn enhance-image [image algorithm pad pad-char]
  (let [bigger-image (pad-image image pad pad-char)]
    (->> bigger-image
         (map-indexed
          (fn [y row]
            (->> row
                 char-array
                 (map-indexed
                  (fn [x pixel]
                    (if (or (= x 0) (= y 0)
                            (= x (dec (count (first bigger-image))))
                            (= y (dec (count bigger-image))))
                      pixel
                      (apply-algorithm algorithm
                                       (pixel-square bigger-image [x y])))))
                 (apply str))))
         vec)))

(defn count-pixels [image]
  (->> (apply concat image)
       char-array
       (reduce (fn [agg c] (+ agg (if (= c \1) 1 0))) 0)))

(defn print-image [image]
  (str/join "\n" image))

; part 1
(println (let [{:keys [image algorithm]} (parse-input "day20.txt")]
           (-> image
               (enhance-image algorithm 2 \0)
               (unpad-image 1)
               (enhance-image algorithm 2 \1)
               (unpad-image 1)
               count-pixels)))

; part 2
(println (let [{:keys [image algorithm]} (parse-input "day20.txt")
               pad-sequence (->> [\0 \1]
                                 repeat
                                 (apply concat)
                                 (take 50))]
           (count-pixels (reduce (fn [image pad]
                                   (-> image
                                       (enhance-image algorithm 2 pad)
                                       (unpad-image 1)))
                                 image pad-sequence))))

;; (println (let [{:keys [_ algorithm]} (parse-input "day20.txt")]
;;            (->> (pad-image ["111" "101" "111"] 5 \0)
;;                 (iterate #(enhance-image % algorithm 0 \0))
;;                 (take 2)
;;                 last
;;                 print-image)))

;; (println (let [{:keys [_ algorithm]} (parse-input "day20.txt")]
;;            (->> (pad-image ["111" "101" "111"] 2 \0)
;;                 (iterate #(enhance-image % algorithm 0 \0))
;;                 (take 2)
;;                 last
;;                 print-image)))

;; (println "===")

;; (println (let [{:keys [_ algorithm]} (parse-input "day20.txt")]
;;            (->> (pad-image ["111" "101" "111"] 5 \1)
;;                 (iterate #(enhance-image % algorithm 0 \1))
;;                 (take 2)
;;                 last
;;                 print-image)))

;; (println (let [{:keys [_ algorithm]} (parse-input "day20.txt")]
;;            (->> (pad-image ["111" "101" "111"] 2 \1)
;;                 (iterate #(enhance-image % algorithm 0 \1))
;;                 (take 2)
;;                 last
;;                 print-image)))
