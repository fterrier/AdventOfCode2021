(require '[clojure.string :as str])

(defn parse-coord-row [row]
  (vec (map #(. Integer parseInt %) (str/split row #","))))

(defn parse-coords [coords]
  (->> (str/split coords #"\n")
       (remove empty?)
       (map parse-coord-row)
       vec))

(defn parse-input [input]
  (let [scanner-coords (str/split (slurp input) #".*scanner.*\n")]
    (->> (map parse-coords scanner-coords)
         (remove empty?))))

; scanner-0 - [ref]=[0]
; ...x...x..x...x.x
; 0123456789

; ..x...x.x...x.
; 0123456789
; overlapping-pos=[8 1] [coord-ref-scanner]
; scanner-pos [pos-ref-scanner - pos-other-scanner]
; beacons-pos=[[10],[14],[16],[20]] [dir * pos + scanner-pos]

(defn build-intervals [positions]
  (->> positions
       sort
       (partition 2 1)
       (map (fn [interval]
              {:size (- (second interval) (first interval))
               :start-pos (first interval)
               :end-pos (second interval)}))))

;; (println (build-intervals [1 4 5]))

(defn all-suffixes [coll]
  (for [start-pos (range (count coll))]
    (drop start-pos coll)))

; checks if ref-intervals contains a prefix of intervals
(defn contains-prefix? [ref-intervals intervals min-overlaps]
  (let [[_ overlaps]
        (loop [ref-intervals ref-intervals
               intervals intervals
               overlaps 1
               ref-size 0
               size 0]
          (if (or (empty? ref-intervals) (empty? intervals)) [nil overlaps]
              (let [new-ref-size (+ ref-size (:size (first ref-intervals)))
                    new-size (+ size (:size (first intervals)))]
                ;; (println ref-size size)
                (cond
                  (= new-size new-ref-size) (recur (drop 1 ref-intervals)
                                                   (drop 1 intervals)
                                                   (inc overlaps) 0 0)
                  (< new-ref-size new-size) (recur (drop 1 ref-intervals)
                                                   intervals overlaps
                                                   new-ref-size
                                                   size)
                  (> new-ref-size new-size) (recur ref-intervals
                                                   (drop 1 intervals) overlaps
                                                   ref-size
                                                   new-size)))))]
    (>= overlaps (dec min-overlaps))))

(defn overlap-pos [ref-intervals intervals min-overlaps]
  (->>
   (for [ref-suffix (all-suffixes ref-intervals) ; TODO remove less than min-overlaps
         suffix (all-suffixes intervals)]
     (if (contains-prefix? ref-suffix suffix min-overlaps)
       (- (:start-pos (first ref-suffix)) (:start-pos (first suffix)))
       nil))
   (remove nil?)
   (into #{})))

; orientation = [[axis dir] [axis dir]]
(defn build-all-intervals [coords orientations]
  (into {} (map (fn [[axis dir :as orientation]]
                  {orientation
                   (build-intervals (map (fn [pos] (* pos dir))
                                         (map #(get % axis) coords)))}) orientations)))

;; (println (build-all-intervals [[1] [2] [5]] [[0 1] [0 -1]]))

(defn try-all-overlaps [ref-coords coords min-overlaps]
  ; [[[axis-x dir] [axis-y dir] [axis-z dir]] [scanner-pos]]
  (let [all-ref-intervals (build-all-intervals ref-coords
                                               [[0 1] [1 1] [2 1]])
        all-intervals (build-all-intervals coords
                                           [[0 1] [0 -1] [1 1] [1 -1] [2 1] [2 -1]])]
    (loop [axis [0 1 2]
           mapping []
           scanner-pos []]
      (if
       (empty? axis) [mapping scanner-pos]
       (let [next-ref-axis (first axis)
             axis-to-try (->> [0 1 2]
                              (remove (fn [a] (contains? (into #{} (map first mapping)) a))))
             [orientation pos]
             (->> axis-to-try
                  (mapcat (fn [a] [[a 1] [a -1]]))
                  (map (fn [orientation]
                         [orientation
                          (overlap-pos (get all-ref-intervals [next-ref-axis 1])
                                       (get all-intervals orientation)
                                       min-overlaps)]))
                  (remove (fn [[_ overlap-pos]] (empty? overlap-pos)))
                  first)]
         (if (nil? pos) nil
             (recur (drop 1 axis)
                    (conj mapping orientation)
                    (conj scanner-pos (first pos)))))))))

;; (println (overlap-pos (build-intervals [0    1   5   10   16])
;;                       (build-intervals [-1   3    8   14]) 4))

;; (println (overlap-pos (build-intervals [1   5   10   16])
;;                       (build-intervals [-2  -1   3    8   14]) 4))

;; (println (overlap-pos (build-intervals [0 4 3]) (build-intervals [-1 -5 -2]) 3))
;; (println (overlap-pos (build-intervals [0 1 3 4 6 7 9])
;;                       (build-intervals [-1 0 1 2 3]) 3))

(defn translate-coord [coord [mapping scanner-pos]]
  (->> (map vector mapping scanner-pos)
       (map (fn [[[a dir] pos]]
              (+ pos (* dir (get coord a)))))
       vec))

;; (println (translate-coord [686 422 578] [[[0 -1] [1 1] [2 -1]] [68 -1246 -43]]))

(defn translate-mapping [m [mapping _]]
  (->> mapping
       (map (fn [[a dir]]
              [(first (get m a)) (* dir (second (get m a)))]))
       vec))

;; (println (translate-mapping [[0 -1] [1 1] [2 -1]]
;;                             [[[0 -1] [1 1] [2 -1]] [68 -1246 -43]]))

(defn translate-coords [coords [mapping scanner-pos]]
  (->> coords
       (map #(translate-coord % [mapping scanner-pos]))))

; scanner 0 1 - [0 -1] [1 1] [2 -1] [68, -1246, -43]
; scanner 0 -618,-824,-621
; scanner 1 686,422,578

; scanner 0 1 - [1 -1] [2 1] [0 -1] [68, -1246, -43]
; scanner 0 -618,-824,-621
; scanner 1 578,686,422 -- x : orientation[0] 

; scanner 1 4 - [1 1] [2 -1] [0 -1] [88, 113, -1104]

; pos = [scanner-dir * pos + scanner-pos]

(defn find-all-positions [scanners]
  (loop [relative-pos {0 [[[0 1] [1 1] [2 1]] [0 0 0]]}
         scanners-used-as-ref #{}
         positions (into #{} (first scanners))]
    (if (= (count relative-pos) (count scanners))
      {:positions positions :scanners relative-pos}
      (let [scanner-indexes (range (count scanners))
            ref-scanner (->> (keys relative-pos)
                             (remove #(contains? scanners-used-as-ref %))
                             first)
            scanners-pos
            (->> scanner-indexes
                 (remove #(contains? relative-pos %))
                 (remove #(= ref-scanner %))
                 (map (fn [index]
                        [index (try-all-overlaps (nth scanners ref-scanner)
                                                 (nth scanners index) 12)]))
                 (remove #(nil? (second %)))
                 (map (fn [[index [mapping scanner-pos]]]
                        [index [(translate-mapping mapping
                                                   (get relative-pos ref-scanner))
                                (translate-coord scanner-pos
                                                 (get relative-pos ref-scanner))]])))]
        (recur (into relative-pos scanners-pos)
               (conj scanners-used-as-ref ref-scanner)
               (into positions
                     (mapcat (fn [[index mapping-pos]]
                               (translate-coords (nth scanners index) mapping-pos)) scanners-pos)))))))

; part 1
(println (let [scanners (parse-input "day19.txt")]
           (->> (find-all-positions scanners)
                :positions
                count)))

(defn manhattan-distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1)) (Math/abs (- z2 z1))))

(defn largest-distance [coords]
  (->> (for [x (range (count coords))
             y (range x (count coords))] [x y])
       (map (fn [[x y]] (manhattan-distance (nth coords x) (nth coords y))))
       (sort >)
       first))

; part 2
(println (let [scanners (parse-input "day20.txt")]
           (->> (find-all-positions scanners)
                :scanners
                vals
                (map second)
                largest-distance)))

;; --- scanner 0 ---
;; 1,2
;; 4,1
;; 5,3

;; --- scanner 1 ---
;; -1,-1
;; -5,0
;; -2,1

;; --- scanner 1 ---
;; 1,-1 
;; 2,1
;; 5,0

; 1,4,5 -> [1,4/3],[4,5/1]
; -5,-2,-1 -> [-5,-2/3],[-2,-1/1]
; inverting: 1,2,5 -> [1,2/1],[2,5/3]
; scanner-pos=[5 1]

; 0,3,4 -> [0,3/3],[3,4/1]
; 1,2,5 -> [1,2/1],[2,5/3]
; inverting: -5,-2,-1 -> [-5,-2/3],[-2,-1/1]
; scanner-pos=[5 -1]

;; scanner 0
;; ...B.
;; B....
;; ....B
;; S....

;; scanner 1
;; ...B..
;; B....S
;; ....B.

;; ...B..
;; B....S
;; ....B.
;; S.....