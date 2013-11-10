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

(solve-with-stepper 4)

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

(solve-stack 10)
