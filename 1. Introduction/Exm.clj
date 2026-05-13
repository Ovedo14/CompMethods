(ns Exm)

(defn fizz-buzz
  [n]
  (loop [li (range 1 (+ n 1))
         r []]
    (cond (= (count li) 0) r
          (= (rem (first li) 15) 0) (recur (rest li) (conj r :fizz-buzz))
          (= (rem (first li) 5) 0) (recur (rest li) (conj r :buzz))
          (= (rem (first li) 3) 0) (recur (rest li) (conj r :fizz))
          :else   (recur (rest li) (conj r (first li))))))

(fizz-buzz 15)


(defn take-alternates
  [n]
  (if (= (count n) 0) ()
                      (loop [i 1
                             li n
                             r []]
                        (cond (= (count li) 0) r
                              (odd? i) (recur (inc i) (rest li) (conj r (first li)))
                              (even? i) (recur (inc i) (rest li) r)))))

(take-alternates [1 2 3 4 5 6])