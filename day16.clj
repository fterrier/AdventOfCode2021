(require '[clojure.string :as str])
(require '[clojure.walk :as walk])

(defn to-int [bits]
  (. Integer parseInt bits 2))

(declare decode-content)

(defn split-string [pos s]
  (let [[f r] (split-at pos s)]
    [(apply str f) (apply str r)]))

;; (println (split-string 2 "12345"))

(defn end-padding? [bits]
  (->> bits
       (remove #(= % \0))
       empty?))

;; (println (end-padding? "0000"))
;; (println (end-padding? "0001"))

(defn parse-packets [bits]
  (if (end-padding? bits) []
      (let [[version r] (split-string 3 bits)
            [taipe r] (split-string 3 r)
            {:keys [remaining] :as content} (decode-content (to-int taipe) r)]
        (lazy-seq
         (cons (assoc content :version (to-int version))
               (parse-packets remaining))))))

(defn parse-literal [bits]
  (let [parts (->> bits
                   (partition 5 5 nil)
                   (map #(apply str %))
                   vec)]
    (loop [[f & r] parts
           decoded ""]
      (let [[c bits] (split-string 1 f)]
        (case c
          "0" {:literal (BigInteger. (str decoded bits) 2)
               :remaining (apply str r)}
          "1" (recur r (str decoded bits)))))))

;; (println (parse-literal "101111111000101000"))

(defn parse-15-bits [bits]
  (let [[length r] (split-string 15 bits)
        [packets r] (split-string (to-int length) r)]
    {:remaining r
     :packets (parse-packets packets)}))

(defn parse-11-bits [bits]
  (let [[num-packets r] (split-string 11 bits)
        packets (->> (parse-packets r)
                     (take (to-int num-packets)))]
    {:packets packets
     :remaining (:remaining (last packets))}))

(defn parse-operator [bits]
  (let [[length-type r] (split-string 1 bits)]
    (case length-type
      "0" (parse-15-bits r)
      "1" (parse-11-bits r))))

(defn decode-content [taipe bits]
  (case taipe
    4 (parse-literal bits)
    (assoc (parse-operator bits) :operator taipe)))

;; (println (parse-packets "110100101111111000101000"))
;; (println (parse-packets "00111000000000000110111101000101001010010001001000000000"))
;; (println (parse-packets "11101110000000001101010000001100100000100011000001100000"))

(defn to-binary [hexa]
  (->> (char-array hexa)
       (reduce (fn [s digit]
                 (->> (str digit)
                      (#(. Integer parseInt % 16))
                      (. Integer toBinaryString)
                      (. Integer parseInt)
                      (format "%04d")
                      (str s)))
               "")
       (apply str)))

;; (println (to-binary "D2FE28")) ; == 1101 0010 1111 1110 0010 1000

(defn sum-version [packet]
  (->> packet
       (tree-seq :packets :packets)
       (map :version)
       (reduce +)))

; part 1
(println (->> (slurp "day16.txt")
              to-binary
              parse-packets
              first
              sum-version))

(defn wrap-cmp [op]
  (fn [x y]
    (if (op x y) 1 0)))

(defn get-fun [operator]
  (case operator
    0 +
    1 *
    2 min
    3 max
    5 (wrap-cmp >)
    6 (wrap-cmp <)
    7 (wrap-cmp =)))

(defn evaluate [node]
  (cond
    (:literal node) (:literal node)
    (:operator node) (apply (get-fun (:operator node)) (:packets node))
    :else node))

; part 2
(println (->> (slurp "day16.txt")
          to-binary
          parse-packets
          first
          (walk/postwalk evaluate)))
