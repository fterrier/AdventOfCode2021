(require '[clojure.string :as str])

(defn parse-input [input]
  (let [rows (str/split (slurp input) #"\n")]
    (map read-string rows)))

(declare mk-leaf)
(declare mk-tree)
(declare mk-node)

(defprotocol Tree
  (value [this])
  (replaces [this new-tree])
  (sibling-right [this])
  (sibling-left [this])
  (leaf-right [this])
  (leaf-left [this])
  (add-value [this n])
  (explode-all [this level])
  (split-leftmost [this])
  (to-vector [this])
  (magnitude [this]))

(deftype Node [left-atom right-atom parent]
  Tree
  (value [this] nil)
  (sibling-right [this]
    (cond (nil? parent) nil
          (identical? @(.left-atom parent) this)
          (leaf-left @(.right-atom parent))
          (identical? @(.right-atom parent) this)
          (sibling-right parent)))
  (sibling-left [this]
    (cond (nil? parent) nil
          (identical? @(.right-atom parent) this)
          (leaf-right @(.left-atom parent))
          (identical? @(.left-atom parent) this)
          (sibling-left parent)))
  (leaf-right [this]
    (leaf-right @right-atom))
  (leaf-left [this]
    (leaf-left @left-atom))
  (replaces [this new-tree]
    (cond (identical? @(.left-atom parent) this)
          (reset! (.left-atom parent) new-tree)
          (identical? @(.right-atom parent) this)
          (reset! (.right-atom parent) new-tree)))
  (explode-all [this level]
    (if (= 4 level)
      (do
        (if-let [sleft (sibling-left this)]
          (add-value sleft (value @left-atom)))
        (if-let [sright (sibling-right this)]
          (add-value sright (value @right-atom)))
        (replaces this (mk-leaf 0 parent))
        true)
      (do (explode-all @left-atom (inc level))
          (explode-all @right-atom (inc level))
          false)))
  (split-leftmost [this]
    (if (split-leftmost @left-atom)
      true
      (split-leftmost @right-atom)))
  (to-vector [this]
    [(to-vector @left-atom) (to-vector @right-atom)])
  (magnitude [this]
    (+ (* 3 (magnitude @left-atom))
       (* 2 (magnitude @right-atom))))

  Object
  (toString [this] (str "[" @left-atom " " @right-atom "]")))

(defn split-num [n]
  [(int (. Math floor (/ n 2))) (int (. Math ceil (/ n 2)))])

(deftype Leaf [value parent]
  Tree
  (value [this] @value)
  (add-value [this n] (swap! value #(+ n %)))
  (replaces [this new-tree]
    (cond (identical? @(.left-atom parent) this)
          (reset! (.left-atom parent) new-tree)
          (identical? @(.right-atom parent) this)
          (reset! (.right-atom parent) new-tree)))
  (leaf-right [this] this)
  (leaf-left [this] this)
  (sibling-right [this]
    (cond (identical? @(.left-atom parent) this)
          (leaf-left @(.right-atom parent))
          (identical? @(.right-atom parent) this)
          (sibling-right parent)))
  (sibling-left [this]
    (cond (identical? @(.right-atom parent) this)
          (leaf-right @(.left-atom parent))
          (identical? @(.left-atom parent) this)
          (sibling-left parent)))
  (explode-all [this level] false)
  (split-leftmost [this]
    (if (<= @value 9)
      false
      (let [[lvalue rvalue] (split-num @value)]
        (replaces this (mk-node lvalue rvalue parent))
        true)))
  (to-vector [this]
    @value)
  (magnitude [this]
    @value)

  Object
  (toString [this] (str @value)))

(defn mk-leaf [value parent]
  (Leaf. (atom value) parent))

(defn mk-node [left right parent]
  (let [^Node tree (Node. (atom nil) (atom nil) parent)
        left-tree (mk-tree left tree)
        right-tree (mk-tree right tree)]
    (reset! (.left-atom tree) left-tree)
    (reset! (.right-atom tree) right-tree)
    tree))

(defn mk-tree [stuff parent]
  (if (vector? stuff)
    (let [[left right] stuff]
      (mk-node left right parent))
    (mk-leaf stuff parent)))

(defn reduce-tree [tree]
  (->> [tree false]
       (iterate (fn [[tree _]]
                  (let [explode-changed (explode-all tree 0)
                        split-changed (split-leftmost tree)]
                    [tree (and (not explode-changed)  (not split-changed))])))
       (drop-while (fn [[_ reduced]] (not reduced)))
       first
       first))

;; (println
;;  (let [tree (mk-tree (read-string "[[[[3,10],4],5],6]") nil)]
;;    (split-leftmost tree)
;;    tree))

;; (println
;;  (let [tree (mk-tree (read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") nil)]
;;    ((explode-all tree 0))
;;    tree))

;; (println
;;  (let [tree (mk-tree (read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") nil)]
;;    (reduce-tree tree)))

; part 1
(println (->> (parse-input "day18.txt")
              (reduce (fn [sum n]
                        (->> (mk-tree [sum n] nil)
                             reduce-tree
                             to-vector)))
              (#(mk-tree % nil))
              magnitude))

(defn all-combinations [v]
  (->> (for [x (range (count v))
             y (range (count v))] [x y])
       (filter (fn [[left right]] (not= left right)))
       (map (fn [[left right]] [(nth v left) (nth v right)]))))

;; ; part 2
(println (->> (parse-input "day18.txt")
              all-combinations
              (map (fn [nums] (->> (mk-tree nums nil)
                                   reduce-tree
                                   magnitude)))
              (sort >)
              first))

;; [
;;   [
;;     [
;;       [4,0],
;;       [5,4]
;;     ],
;;     [
;;       [7,7],
;;       [6,0]
;;     ]
;;   ],
;;   [
;;     [8,
;;       [7,7]
;;     ],
;;     [
;;       [7,9],
;;       [5,0]
;;     ]
;;   ]
;; ]

; 1. explode all pairs with level > 4
; 2. split leftmost all pairs until level > 4
; 3. go to 1.
