(ns queens)

;(defn progress-state [state queens]
;  "a state looks like this: [0 1 2 3 4 5] on a 6-queens problem, get the next step"
;  (loop [i 0 [x & xs] (reverse state) s [] r true]
;    (if (and r (not (nil? x)))
;      (let [[n r-n] (if (= x (last (queens i)))
;                        [(first (queens i)) true]
;                        [(inc x) false])]
;        (recur (inc i) xs (conj s n) r-n))
;      (vec (reverse (concat s  (if (nil? x) xs (cons x xs))))))))
;
; (defn progress-state [state queens]
;  "a state looks like this: [0 1 2 3 4 5] on a 6-queens problem, get the next step"
;  (loop [i 0 [x & xs] (reverse state) s [] r true]
;    (if (and r (not (nil? x)))
;      (let [[n r-n] (if (= x (last (queens i)))
;                        [(first (queens i)) true]
;                        [(inc x) false])]
 ;       (recur (inc i) xs (conj s n) r-n))
 ;     (vec (reverse (concat s  (if (nil? x) xs (cons x xs))))))))

(defn solve [n]
  "solves the queen-problem for a given n"
  (let [domain-initial (vec (for [i (range n)] (vec (range n))))
        constrains [(fn [state] (= (count (distinct state)) (count state)))
                    (fn [state] (let [j (dec (count state))]
                                  (every? true? (for [i (range 0 j)]
                                    (not= (Math/abs (- i j))
                                          (Math/abs (- ((domain-initial i) (nth state (- j i)))
                                                       ((domain-initial j) (first state)))))))))]
        progress-state (fn [state queens]
                         (loop [state state]
                           (if (every? true? (map #(% state) constrains))
                             state
                             (recur (loop [state state]
                                      (if (= (inc (first state)) (count (queens (dec (count state)))))
                                          (recur (rest state))
                                          (cons (inc (first state)) (rest state))))))))
        state-initial []]
    (loop [state state-initial domain domain-initial]
      (if (= n (count state))
        state
        (recur (progress-state (cons 0 state) domain-initial) domain)))))

(solve 14)
