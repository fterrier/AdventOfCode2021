(require '[clojure.string :as str])

(defn parse-bits [bit-str]
  (vec (map (fn [c] (case c \0 :zero \1 :one :unknown))
            (char-array bit-str))))

(defn as-vec [input]
  (with-open [rdr (clojure.java.io/reader input)]
    (vec (map parse-bits (line-seq rdr)))))

(defn count-col [v]
  (reduce (fn [agg bit] (assoc agg bit (inc (get agg bit 0)))) {} v))

(defn rate [[{:keys [zero one]} & remaining-cnts] v]
  (cons (case v
          :least (if (> zero one) :one :zero)
          :most (if (> one zero) :one :zero))
        (if (empty? remaining-cnts) [] (rate remaining-cnts v))))

(defn to-int [v]
  (. Integer parseInt (apply str (map #(case % :one \1 :zero \0) v)) 2))

; part 1
(println
 (let [cnts (map count-col (apply mapv vector (as-vec "day3.txt")))]
   {:epsilon (to-int (rate cnts :least))
    :gamma (to-int (rate cnts :most))}))

(defn keep-rows [rows index v]
  (filter #(= (get % index) v) rows))

(defn rating [rows index mode]
  (if (= (count rows) 1)
    (first rows)
    (let [cnt
          (count-col (get (apply mapv vector rows) index))
          matching-rows
          (cond
            (<= (:zero cnt) (:one cnt))
            (keep-rows rows index (case mode :most :one :least :zero))
            (> (:zero cnt) (:one cnt))
            (keep-rows rows index (case mode :most :zero :least :one)))]
      (rating matching-rows (inc index) mode))))

; part 2
(println
 (let [rows (as-vec "day3.txt")]
   {:oxygen (to-int (rating rows 0 :most))
    :co2-scrubber (to-int (rating rows 0 :least))}))