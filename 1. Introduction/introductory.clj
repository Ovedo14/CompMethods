;----------------------------------------------------------
; Problem Set #1: Introductory Exercises
; Date: February 20, 2026.
; Authors:
;          A01803450 Alexa Sarahi Castillo Portillo
;          A01803272 Ricardo Oved Cornejo Castro
;----------------------------------------------------------

(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

; This is a comment

(defn gibibytes->bytes
  "Returns ... "
  [gibibyte]
  (* gibibyte 1024 1024 1024))
(gibibytes->bytes 5)

(defn fahrenheit->celsius
  "Returns ... "
  [Farenheit]
  (/ (* 5.0 (- Farenheit 32.0)) 9.0 ))

(defn sign
  "Returns ... "
  [n]
  (if (< n 0) -1
  (if (> n 0) 1 0)))


(defn roots
  "Return ..."
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b) (* 4 a c)))
        f (* 2 a)
x1 (/ (+ d e) f)
x2 (/ (- d e) f)]
[x1 x2]))



(defn bmi
  "Returns ... "
  [weight height]
  (let [bmi (/ weight (* height height))]
    (cond
      (< bmi 18.5) 'underweight
      (< bmi 25) 'normal
      (< bmi 30) 'obese1
      (< bmi 40) 'obese2
      :else 'obese3)))

(defn type-of-triangle
  "Returns ... "
  [a b c]
  (cond
    (and (= a b) (= b c)) 'equilateral
    (or (and (= a b) (not= b c))
        (and (= a c) (not= a b))
        (and (= b c) (not= a b))) 'isosceles
    :else 'scalene))

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

(deftest test-fahrenheit->celsius
  (is (= 100.0 (fahrenheit->celsius 212.0)))
  (is (= 0.0 (fahrenheit->celsius 32.0)))
  (is (= -40.0 (fahrenheit->celsius -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(deftest test-type-of-triangle
  (is (= 'equilateral (type-of-triangle 3 3 3)))
  (is (= 'equilateral (type-of-triangle 4.2 4.2 4.2)))
  (is (= 'isosceles (type-of-triangle 4 4 3)))
  (is (= 'isosceles (type-of-triangle 4 3 4)))
  (is (= 'isosceles (type-of-triangle 3 4 4)))
  (is (= 'scalene (type-of-triangle 1 2 3)))
  (is (= 'scalene (type-of-triangle 7.1 6.4 9.2))))
(run-tests)