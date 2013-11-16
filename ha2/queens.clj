(ns queens)

;; to count the number of nodes
(def counter (atom 0))

(defn solve-stack [n]
  "exacly the algorithm mentioned in the exercise"
  (let [domains (vec (for [i (range n)] (vec (range n))))
        constrains [(fn [state] (= (count (distinct state)) (count state)))
                    (fn [state] (let [j (dec (count state))]
                                  (every? true? (for [i (range j)]
                                                  (not= (Math/abs (- i j))
                                                        (Math/abs (- ((domains i) (nth state (- j i)))
                                                                     ((domains j) (first state)))))))))]]
    (loop [[x & xs] [[]]]
      (if (or (nil? x) (= n (count x)))
        x
        (recur (concat (for [i (domains (count x))
                             :when (every? true? (map #(% (cons i x)) constrains))]
                         (do (swap! counter inc)
                             (cons i x))) xs))))))


(defn solve-stack-fc-mrv [n]
  "exacly the algorithm mentioned in the exercise with forward checking and mrv"
  (let [domains-initial (zipmap (range n) (repeat (range n)))
        constrains [(fn [[x1 y1] [x2 y2]] (not= y1 y2))
                    (fn [[x1 y1] [x2 y2]]
                      (not= (Math/abs (- x1 x2))
                            (Math/abs (- y1 y2))))]
        forward-check (fn [x1 y1 domains]
                        (apply merge
                               (for [[x2 domain] domains]
                                 {x2 (filter (fn [y2] (every? true? (map #(% [x1 y1] [x2 y2]) constrains))) domain)})))]
    (loop [[[state domains] & xs] [[{} domains-initial]]]
      (if (or (nil? state) (= n (count state)))
        (map second (sort state))
        (let [[x domain] (rand-nth ((comp second first) (sort (group-by (comp count second) domains))))
              domains (dissoc domains x)
              states (filter #(every? (comp (partial < 0) count) (second %))
                             (for [y domain]
                               [(assoc state x y) (forward-check x y domains)]))]
          (recur (do (swap! counter (partial + (count states)))
                     (concat states xs))))))))

(defn solve-stack-arc-mrv [n]
  "exacly the algorithm mentioned in the exercise with arc-consistency and mrv"
  (let [domains-initial (zipmap (range n) (repeat (range n)))
        constrains [(fn [[x1 y1] [x2 y2]] (not= y1 y2))
                    (fn [[x1 y1] [x2 y2]]
                      (not= (Math/abs (- x1 x2))
                            (Math/abs (- y1 y2))))]
        arc-consistency (fn [domains]
                          (let [arcs
                                (for [[x1 d1] domains
                                      [x2 d2] domains
                                      :when (not= x1 x2)]
                                  [x1 x2])]
                            (loop [[[x1 x2] & xs] arcs domains domains]
                              (if (nil? x1)
                                domains
                                (let [d1 (domains x1)
                                      d2 (domains x2)
                                      d1-new (for [y1 d1
                                                   :when (not (empty? (filter (fn [y2]
                                                                   (every? true? (map #(% [x1 y1]
                                                                                          [x2 y2])
                                                                                      constrains))) d2)))] y1)
                                      new-arcs (for [[x3 y3] domains
                                                     :when (and (not= x3 x2) (not= x3 x1))] [x3 x1])]
                                  (if (not= (sort d1-new) (sort d1))
                                    (recur (concat xs new-arcs) (assoc domains x1 d1-new))
                                    (recur xs domains)))))))]
    (loop [[[state domains] & xs] [[{} domains-initial]]]
      (if (or (nil? state) (= n (count state)))
        (map second (sort state))
        (let [[x domain] (rand-nth ((comp second first)
                                    (sort (group-by (comp count second)
                                                    (filter (fn [[x d]] (not (state x))) domains)))))
              states (for [y domain
                           :let [domains-new (arc-consistency (assoc domains x [y]))]
                           :when (empty? (filter (fn [[x d]] (zero? (count d))) domains-new))]
                       [(assoc state x y) domains-new])
              states states]
          (recur (do (swap! counter (partial + (count states)))
                     (concat states xs))))))))


(defmacro time
  "measures the time in msecs and returns it in a vector with the value of the expr"
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
      [(/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#]))


(defn test-suite []
  (let [funcs [solve-stack solve-stack-fc-mrv solve-stack-arc-mrv]
        min-n 0
        max-n 30
        max-time (* 120 1000.0)]
    (loop [[f & fs] funcs results {}]
      (if (nil? f) results
          (recur fs
                 (assoc results
                   (str f)
                   (loop [[x & xs] (range min-n (inc max-n)) result []]
                     (do (reset! counter 0)
                         (println (str f) " - " (str x))
                         (let [[t r] (time (f x))
                               result (conj result {:n x :time t :result r :nodes @counter})]
                           (if (or (> t max-time) (empty? xs))
                             result
                             (recur xs result)))))))))))
(test-suite)
