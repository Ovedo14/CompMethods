;----------------------------------------------------------
; Problem Set #5: Parallel Programming
; Date: April 10, 2026.
; Authors:
;          A01803450 Alexa Sarahi Castillo Portillo
;          A01803272 Ricardo Oved Cornejo Castro
;----------------------------------------------------------
(ns parallelism)

;Problem 1	n = 150000
;Run	T1	T24
;1	5912.2757	481.7832
;2	5749.7214	470.3893
;3	5742.8663	458.2213
;4	5719.4072	470.6485
;5	5794.4897	529.6548
;Average	5783.75206	482.13942
;S24:	11.99601572


(defn bits
  "Counts the number of set bits (ones) in the binary representation of a number."
  [x]
  (.bitCount (biginteger x)))

(defn fact-seq
  "Calculates the factorial of n sequentially and returns the number of bits in the result."
  [n]
  (loop [i 1
         r 1]
    (if (> i n)
      (bits r)
      (recur (inc i)
             (*' r i)))))

(defn fact-partial
  "Calculates the partial product of numbers in a given range [start end)."
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i)
             (*' r i)))))

(defn fact-ranges
  "Divides the range from 1 to n into p sub-ranges for parallel processing."
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p)) [(inc n)])))

(defn fact-par
  "Calculates the factorial of n in parallel and returns the number of bits in the result."
  [n]
  (let [p (.availableProcessors (Runtime/getRuntime))]
    (bits (reduce *'
                  (pmap fact-partial
                        (fact-ranges n p))))))


;(def n 150000)
;(time (fact-seq n))
;(time (fact-par n))


;Problem 2
;Problem 2	n = 1000000000
;Run	T1	T24
;1	27429.6484	19248.9619
;2	27339.5779	19492.1699
;3	27184.7339	19248.9619
;4	28676.3843	19791.5548
;5	28097.3017	19735.7816
;Average	27745.52924	19503.48602
;S24:	1.422593336

(defn compute-pi-seq
  "Computes an approximation of PI sequentially using numerical integration."
  [n]
  (let [width (/ 1.0 n)]
    (loop [i 0
           sum 0.0]
      (if (< i n)
        (let [mid (* (+ i 0.5) width)
              height (/ 4.0 (+ 1.0 (* mid mid)))]
          (recur (inc i) (+ sum height)))
        (* sum width)))))

(defn pi-partial
  "Computes a partial sum for the PI approximation over a given range [start end)."
  [start end width]
      (loop [i start
             sum 0.0]
            (if (< i end)
              (let [mid (* (+ i 0.5) width)
                    height (/ 4.0 (+ 1.0 (* mid mid)))]
                   (recur (inc i) (+ sum height)))
              sum)))

(defn compute-pi-par
  "Computes an approximation of PI in parallel using numerical integration."
  [n]
  (let [p     (.availableProcessors (Runtime/getRuntime))
        width (/ 1.0 n)
        ranges (fact-ranges n p)]
       (* width
          (reduce +
                  (pmap (fn [[start end]] (pi-partial start end width))
                        ranges)))))

(def n 10000000000)
(time (compute-pi-seq n))
(time (compute-pi-par n))



;Problem 3
;Problem 3	n = 30
;Run	T1	T24
;1	93225.9793	25073.5486
;2	90708.6319	25257.63
;3	89632.9861	25241.4576
;4	90922.9035	25338.1413
;5	90034.2708	25338.1413
;Average	90904.95432	25343.6017
;S24:	3.586899581

(defn bin-hex-palindromes-seq
  "Sequentially counts the numbers up to 2^n that are palindromes in both binary and hexadecimal representations."
  [n]
  (let [limite (bit-shift-left 1 n)]
    (count
      (filter (fn [i]
                (let [bin (Integer/toBinaryString i)
                      hex (Integer/toHexString i)]
                  (and (= bin (clojure.string/reverse bin))
                       (= hex (clojure.string/reverse hex)))))
              (range limite)))))

(defn bin-hex-palindrome?
  "Checks if a given number is a palindrome in both binary and hexadecimal representations."
  [i]
  (let [bin (Integer/toBinaryString i)
        hex (Integer/toHexString i)]
    (and (= bin (clojure.string/reverse bin))
         (= hex (clojure.string/reverse hex)))))

(defn count-palindromes-in-range
  "Counts the binary-hexadecimal palindromes in a given range [start end)."
  [[start end]]
  (count
    (filter bin-hex-palindrome?
            (range start end))))

(defn fact-ranges
  "Generates partition ranges for parallel processing, specifically for starting from 0."
  [n p]
  (let [step (quot n p)]
    (partition 2 1 (concat (range 0 n step) [n]))))

(defn bin-hex-palindromes-par
  "In parallel, counts the numbers up to 2^n that are palindromes in both binary and hexadecimal representations."
  [n]
  (let [p      (.availableProcessors (Runtime/getRuntime))
        limite (bit-shift-left 1 n)
        ranges (fact-ranges limite p)]
    (reduce + (pmap count-palindromes-in-range ranges))))

;(def n 30)
;(time (bin-hex-palindromes-seq n))
;(time (bin-hex-palindromes-par n))

; Problem 4  n = 800000
; Run #    T1           T24
; 1        4523.082846  1084.900641
; 2        4478.193253  907.292667
; 3        4519.969959  920.617177
; 4        4402.29829   942.490745
; 5        4425.408484  942.619227
; Average  4469.790566  959.5840914
;
; S24:  4.658049885

(defn create-random-data
  "Generates a sequence of n random integers between 0 and 999."
  [n]
  (repeatedly n #(rand-int 1000)))

; (create-random-data 100)

(defn insertion-sort
  "Sorts a sequence using the insertion sort algorithm."
  [s]
  (loop [s s
         r ()]
    (if (empty? s)
      r
      (let [x              (first s)
            [before after] (split-with #(< % x) r)]
        (recur (rest s)
               (concat before [x] after))))))

;(apply <= (insertion-sort (create-random-data 1000)))

(defn merge-algorithm
  "Merges two sorted collections into a single sorted collection."
  [a b]
  (loop [a a
         b b
         r []]
    (cond
      (empty? a)
      (concat r b)

      (empty? b)
      (concat r a)

      (< (first a) (first b))
      (recur (rest a)
             b
             (conj r (first a)))

      :else
      (recur a
             (rest b)
             (conj r (first b))))))

; (merge-algorithm [1 4 6 9] [2 3 5 7 8 10])

(defn hybrid-sort-seq
  "Sequentially sorts a collection using a hybrid approach of insertion sort and merge sort."
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-algorithm (hybrid-sort-seq a)
                       (hybrid-sort-seq b)))))

(defn hybrid-sort-par
  "In parallel, sorts a collection using a hybrid approach of insertion sort and merge sort."
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm (pmap hybrid-sort-par splitted)))))

;(def n 800000)
;(time (apply <= (hybrid-sort-seq (create-random-data n))))
;(time (apply <= (hybrid-sort-par (create-random-data n))))
