;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: March 27, 2026.
; Authors:
;          A01803450 Alexa Sarahi Castillo Portillo
;          A01803272 Ricardo Oved Cornejo Castro
;----------------------------------------------------------

(ns higher-order-functions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [approx=]]))

; Problem 1
(defn argswap
  "Takes a binary function and returns a new function that swaps the argument order."
  [fun]
  (fn [x y] (fun y x)))

; Problem 2
(defn there-exists-one
  "Returns true if exactly one element in the sequence satisfies the predicate."
  [pred? s]
  (= 1 (count (filter pred? s))))

; Problem 3
(defn linear-search
  "Searches for element x in vector vct using eq-fun for equality comparison.
   Returns the index of the first matching element or nil if not found."
  [vct x eq-fun]
  (loop [i 0]
    (cond
      (= i (count vct))     nil
      (eq-fun x (vct i))    i
      :else                 (recur (inc i)))))

(defn deriv
  "Returns a function that computes the numerical derivative of function f.
   Uses finite differences with step size h (default) or h1 (if provided)."
  [f h]
  (fn
    ([x]
     (/ (- (f (+ x h)) (f x)) h))

    ([x h1]
     (/ (- (f (+ x h1)) (f x)) h1))))

(defn newton
  "Finds a root of function f using Newton's method with n iterations.
   Returns 0 for n <= 0, otherwise uses the Newton-Raphson formula."
  [f n]
  (if (<= n 0) 0
               (let [x-prev (newton f (dec n))
                     df     (deriv f 0.0001)]
                 (- x-prev (/ (f x-prev) (df x-prev))))))

(defn integral
  "Computes the definite integral of function f from a to b using Simpson's rule.
   Divides the interval into n subintervals for numerical integration."
  [a b n f]
  (let [h (/ (- b a) n)
        terminos (map (fn [k]
                        (let [coef (cond (or (= k 0) (= k n)) 1
                                         (odd? k) 4
                                         :else 2)
                              xk (+ a (* k h))]
                          (* coef (f xk))))
                      (range (inc n)))]
    (* (/ h 3) (reduce + terminos))))

(defn binary-search
  "Performs binary search on a sorted vector vct for element incognita.
   Uses lt-fun for less-than comparisons. Returns the index if found, nil otherwise."
  [vct incognita lt-fun]
  (loop [low 0
         high (dec (count vct))]
    (if (> low high)
      nil
      (let [mid (quot (+ low high) 2)
            guess (nth vct mid)]
        (cond
          (not (or (lt-fun incognita guess) (lt-fun guess incognita)))
          mid
          (lt-fun incognita guess)
          (recur low (dec mid))

          :else
          (recur (inc mid) high))))))


(deftest test-argswap
  (is (= '(2 1)
         ((argswap list) 1 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (approx= 75 (df 5) 0.05))
  (is (approx= 30 (ddf 5) 0.05))
  (is (approx= 6 (dddf 5) 0.05)))

(deftest test-newton
  (is (approx= 10.0
               (newton (fn [x] (- x 10))
                       1)
               0.00001))
  (is (approx= -0.5
               (newton (fn [x] (+ (* 4 x) 2))
                       1)
               0.00001))
  (is (approx= -1.0
               (newton (fn [x] (+ (* x x x) 1))
                       50)
               0.00001))
  (is (approx= -1.02987
               (newton (fn [x] (+ (Math/cos x)
                                  (* 0.5 x)))
                       5)
               0.00001)))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))

(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))

(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))


(run-tests)
