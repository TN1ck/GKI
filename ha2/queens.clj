(ns queens)

(defn solve-with-stepper [n]
  "use a stepper instead of a stack, performs better"
  (let [domains-initial (vec (for [i (range n)] (vec (range n))))
        constrains [(fn [state] (= (count (distinct state)) (count state)))
                    (fn [state] (let [j (dec (count state))]
                                  (every? true? (for [i (range 0 j)]
                                    (not= (Math/abs (- i j))
                                          (Math/abs (- ((domains-initial i) (nth state (- j i)))
                                                       ((domains-initial j) (first state)))))))))]
        progress-state (fn [state domains]
                         (loop [state state]
                           (if (every? true? (map #(% state) constrains))
                             state
                             (recur (loop [state state]
                                      (if (= (inc (first state)) (count (domains (dec (count state)))))
                                          (recur (rest state))
                                          (cons (inc (first state)) (rest state))))))))
        state-initial []]
    (loop [state state-initial domains domains-initial]
      (if (= n (count state))
        state
        (recur (progress-state (cons 0 state) domains-initial) domains)))))

(solve-with-stepper 5)


(defn solve-stack [n]
  "exacly the algorithm mentioned in the exercise"
  (let [domains (vec (for [i (range n)] (vec (range n))))
        stack [[]]
        constrains [(fn [state] (= (count (distinct state)) (count state)))
                    (fn [state] (let [j (dec (count state))]
                                  (every? true? (for [i (range 0 j)]
                                                  (not= (Math/abs (- i j))
                                                        (Math/abs (- ((domains i) (nth state (- j i)))
                                                                     ((domains j) (first state)))))))))]]
    (loop [[x & xs] stack]
      (if (or (nil? x) (= n (count x)))
        x
        (recur (concat (for [i (domains (count x))
                             :when (every? true? (map #(% (cons i x)) constrains))]
                         (cons i x)) xs))))))

(solve-stack 8)


(defn solve-stack-fc-mrv [n]
  "exacly the algorithm mentioned in the exercise with forward checking and mrv"
  (let [domains-initial (zipmap (range n) (repeat (range n)))
        stack [[{} domains-initial]]
        constrains [(fn [[x1 y1] [x2 y2]] (not= y1 y2))
                    (fn [[x1 y1] [x2 y2]]
                      (not= (Math/abs (- x1 x2))
                            (Math/abs (- y1 y2))))]
        forward-check (fn [x1 y1 domains]
                        (apply merge
                               (for [[x2 domain] domains]
                                 {x2 (filter (fn [y2] (every? true? (map #(% [x1 y1] [x2 y2]) constrains))) domain)})))]
    (loop [[[state domains] & xs] stack]
      (if (or (nil? state) (= n (count state)))
          (map second (sort state))
          (let [[x domain] (rand-nth ((comp second first) (sort (group-by (comp count second) domains))))
                domains (dissoc domains x)
                states (for [y domain]
                         [(assoc state x y) (forward-check x y domains)])]
            (recur (concat states xs)))))))

(solve-stack-fc-mrv 5)

(defn solve-stack-arc-mrv [n]
  "exacly the algorithm mentioned in the exercise with arc-consistency and mrv"
  (let [domains-initial (zipmap (range n) (repeat (range n)))
        stack [[{} domains-initial]]
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
                              (if (or (nil? x1))
                                domains
                                (let [d1 (domains x1)
                                      d2 (domains x2)
                                      d1-new (loop [[y1 & ys] d1 d1new []]
                                               (if (nil? y1)
                                                 d1new
                                                 (let [xy (for [y2 d2]
                                                            [[x1 y1] [x2 y2]])
                                                       xy1 (map (comp second first)
                                                                (filter (fn [[[x1 y1] [x2 y2]]]
                                                                          (every? true? (map #(% [x1 y1] [x2 y2]) constrains)))
                                                                        xy))
                                                       xy1 xy1]
                                                   (if (empty? xy1)
                                                     (recur ys d1new)
                                                     (recur ys (cons y1 d1new))))))
                                      new-arcs (for [[x3 y3] domains
                                                     :when (and (not= x3 x2) (not= x3 x1))] [x3 x1])
                                      new-arcs new-arcs
                                      xs xs]
                                  (if (not= (sort d1-new) (sort d1))
            (recur (concat xs new-arcs) (assoc domains x1 d1-new))
            (recur xs domains)))))))]
    (loop [[[state domains] & xs] stack]
      (if (or (nil? state) (= n (count state)))
        (map second (sort state))
        (let [[x domain] (rand-nth ((comp second first) (sort (group-by (comp count second) (filter (fn [[x d]] (not (state x))) domains)))))
              states (for [y domain]
                       [(assoc state x y) (arc-consistency (assoc domains x [y]))])
              states states]
          (recur (concat states xs)))))))

(solve-stack-arc-mrv 8)
