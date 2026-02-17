(ns simple_examples)

(defn !
  "Returns ... "
  [n]
  (if (zero? n) 1
    (* n (! (dec n)))))

(! 0)
(! 2)
(! 3)
(! 4)
(! 5)
(! 20)
(! 30)