;----------------------------------------------------------
; Conjunto de problemas n.° 6: Autómatas
; Fecha: 20 de abril de 2026.
; Autores:
; A01803450 Alexa Sarahi Castillo Portillo
; A01803272 Ricardo Oved Cornejo Castro
;----------------------------------------------------------

(ns automata
  "Namespace for the definition and evaluation of Deterministic Finite Automata (DFA)."
  (:require [clojure.test :refer [deftest is run-test run-tests]]))

(defrecord ^{:doc "Record representing a Deterministic Finite Automaton (DFA) with initial state, accept states, and transitions."}
  DFA [initial-state
       accept-states
       transitions])

(defn accepts?
  "Evaluates whether a given DFA accepts the provided input string, processing symbols one by one according to the automaton's transitions."
  [{:keys [initial-state accept-states transitions]} input]
  (loop [input input
         current-state initial-state]
    (if (empty? input)
      (contains? accept-states current-state)
      (recur (rest input)
             (get-in transitions [current-state (first input)])))))

(def dfa-1
  "Deterministic Finite Automaton that accepts strings over the alphabet {a, b} containing the substring 'ab'."
  (->DFA :q0
         #{:q2}
         {:q0 {\a :q1
               \b :q0}
          :q1 {\a :q1
               \b :q2}
          :q2 {\a :q2
               \b :q2}}))

(deftest test-problem1
  (is (accepts? dfa-1 "ab"))
  (is (accepts? dfa-1 "abba"))
  (is (accepts? dfa-1 "aaab"))
  (is (accepts? dfa-1 "abbbbbbbbb"))
  (is (not (accepts? dfa-1 "")))
  (is (not (accepts? dfa-1 "a")))
  (is (not (accepts? dfa-1 "baa")))
  (is (not (accepts? dfa-1 "bbba"))))

(def dfa-2
  "Deterministic Finite Automaton that accepts strings over the alphabet {0, 1} starting with '0' and ending with '1'."
  (->DFA :q0
         #{:q2}
         {:q0         {\0 :q1
                       \1 :dead-state}
          :q1         {\0 :q1
                       \1 :q2}
          :q2         {\0 :q1
                       \1 :q2}
          :dead-state {\0 :dead-state
                       \1 :dead-state}}))

(deftest test-problem2
  (is (accepts? dfa-2 "01"))
  (is (accepts? dfa-2 "0101"))
  (is (accepts? dfa-2 "01111"))
  (is (accepts? dfa-2 "000001"))
  (is (not (accepts? dfa-2 "")))
  (is (not (accepts? dfa-2 "00")))
  (is (not (accepts? dfa-2 "1001011")))
  (is (not (accepts? dfa-2 "1001010"))))

(def dfa-3
  "Deterministic Finite Automaton that accepts strings over the alphabet {x, y} containing the substring 'yyy'."
  (->DFA :q0
         #{:q3}
         {:q0 {\x :q0
               \y :q1}
          :q1 {\x :q0
               \y :q2}
          :q2 {\x :q0
               \y :q3}
          :q3 {\x :q3
               \y :q3}}))

(deftest test-problem3
  (is (accepts? dfa-3 "yyy"))
  (is (accepts? dfa-3 "xyxyyyx"))
  (is (accepts? dfa-3 "xxxxxyyyyy"))
  (is (accepts? dfa-3 "yyyxxxxyyy"))
  (is (not (accepts? dfa-3 "")))
  (is (not (accepts? dfa-3 "xxx")))
  (is (not (accepts? dfa-3 "yxxyxxy")))
  (is (not (accepts? dfa-3 "xyxyyxyyx"))))

(def dfa-4
  "Deterministic Finite Automaton that accepts strings over the alphabet {i, j, k} of even length."
  (->DFA :q0
         #{:q0}
         {:q0 {\i :q1
               \j :q1
               \k :q1}
          :q1 {\i :q0
               \j :q0
               \k :q0}}))

(deftest test-problem4
  (is (accepts? dfa-4 ""))
  (is (accepts? dfa-4 "ji"))
  (is (accepts? dfa-4 "iiiijjjjkkkk"))
  (is (accepts? dfa-4 "kjikjikjikjikjikjikjikji"))
  (is (not (accepts? dfa-4 "i")))
  (is (not (accepts? dfa-4 "ijk")))
  (is (not (accepts? dfa-4 "jjjjjiiiiikkkkk")))
  (is (not (accepts? dfa-4
                     "kjikjikjikjikjikjikjikjikji"))))

(def dfa-5
  "Deterministic Finite Automaton that accepts strings over the alphabet {s, t} where there are no two consecutive 's's nor two consecutive 't's (alternating strings)."
  (->DFA :q0
         #{:q0 :q1 :q2}
         {:q0 {\s :q1
               \t :q2}
          :q1 {\s :q3
               \t :q2}
          :q2 {\s :q1
               \t :q3}
          :q3 {\s :q3
               \t :q3}}))

(deftest test-problem5
  (is (accepts? dfa-5 ""))
  (is (accepts? dfa-5 "s"))
  (is (accepts? dfa-5 "stststs"))
  (is (accepts? dfa-5 "tststststststs"))
  (is (not (accepts? dfa-5 "ss")))
  (is (not (accepts? dfa-5 "ststststt")))
  (is (not (accepts? dfa-5
                     "tstststsststststsssts")))
  (is (not (accepts? dfa-5
                     "tttttttttttttttttttttttt"))))

(def dfa-6
  "Deterministic Finite Automaton that accepts strings over the alphabet {#, $} where each '#' character is followed by exactly one or three '$' characters."
  (->DFA :q0
         #{:q0 :q2 :q4}
         {:q0 {\# :q1
               \$ :q0}
          :q1 {\# :q5
               \$ :q2}
          :q2 {\# :q1
               \$ :q3}
          :q3 {\# :q5
               \$ :q4}
          :q4 {\# :q1
               \$ :q5}
          :q5 {\# :q5
               \$ :q5}}))

(deftest test-problem6
  (is (accepts? dfa-6 ""))
  (is (accepts? dfa-6 "$$$"))
  (is (accepts? dfa-6 "$$$$$$$#$#$$$#$"))
  (is (accepts? dfa-6 "#$$$#$#$$$#$#$$$#$#$"))
  (is (not (accepts? dfa-6 "#")))
  (is (not (accepts? dfa-6 "$$#$#$$#$$$")))
  (is (not (accepts? dfa-6 "$$$$$#$###$$$$#")))
  (is (not (accepts? dfa-6 "#$#$#$#$#$$$#$$$#$$$#"))))

(def dfa-7
  "Deterministic Finite Automaton that accepts strings over the alphabet {@, %} containing exactly two '@' characters."
  (->DFA :q0
         #{:q2}
         {:q0 {\@ :q1
               \% :q0}
          :q1 {\@ :q2
               \% :q1}
          :q2 {\@ :q3
               \% :q2}
          :q3 {\@ :q3
               \% :q3}}))

(deftest  test-problem7
  (is (accepts?  dfa-7  "@@" ))
  (is (accepts?  dfa-7  "%@%@%" ))
  (is (accepts?  dfa-7  "@%%%%%%%%%@%%" ))
  (is (accepts?  dfa-7  "%%%%%%@@%%%%%%%%%%" ))
  (is (not (accepts?  dfa-7  "" )))
  (is (not (accepts?  dfa-7  "%@%" )))
  (is (not (accepts?  dfa-7  "@@@@@@@@@@@@" )))
  (is (not (accepts?  dfa-7  "@%%%%@%%%%%@%%%" ))))

(run-tests)