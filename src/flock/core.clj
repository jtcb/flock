(ns flock.core
  (:require [clojure.pprint :as pprint]))

;; tunable paramters of the gp system

(def default-depth
  "Default maximum depth of a randomly grown tree.
   Currently unused."
  10)

(def default-p-leaf
  "Default probability of a generating a leaf when growing a tree."
  0.5)

(def params 
  "Formal parameters of generated gp functions."
  ['x 'y 'z])

(def terminals
  "Terminals of generated gp functions."
  [0.5 1 2 '(rand)])

(def leaves (into params terminals))

;; gp functions

(defn div
  "Protected division. Division by zero returns 1."
  [x y]
  (if (= y 0)
    1
    (/ x y)))

(defn *-1
  "Unary minus. Note that ordinary (-) is assumed to be binary subtraction."
  [x]
  (- x))

(defn qif
  "Quaternary if. Evaluates to c if a <= b, otherwise, evaluates to d."
  [a b c d]
  (if (<= a b)
    c
    d))

;; TODO: Add more ops; max/min, average, ... ?
(def ops
  "Map of operations that can appear in a generated gp code to their arities"
  {'*-1 1, '+ 2, '- 2, '* 2 'div 2, 'qif 4})


;; code-vector utility functions

(defn pprint-code
  "Pretty-print code (i.e. with indentation and newlines)."
  [code]
  (pprint/with-pprint-dispatch pprint/code-dispatch
    (pprint/pprint code)))

(defn listify
  "Recursively convert nested collection v into nested lists."
  [v]
  (if (coll? v)
    (list* (map listify v))
    v))

(defn to-code
  "Convert nested collection to (unevaluated) anonymous function.
   Suitable for viewing a code-vector via (println) or (pprint-code)
   Requires params (vector of quoted symbols. e.g. ['x 'y]) to be defined."
  [v]
  (let [body (list (listify v))
        full (conj body params 'fn)]
    full)) 

(defn to-lambda 
  "Convert nested collection to anonymous function for sumary execution.
   Requires params (vector of quoted symbols. e.g. ['x 'y]) to be defined."
  [v]
  (eval (to-code v)))


(defn roulette
  "Select one of m's keys with probability proportional to its value.
   e.g. (roulette {:a 1 :b 2}) will select :a with probability 1/3."
  [m]
  (let [total (reduce + (map second m))]
    (loop [i (rand total)
           [[choice weight] & remaining] (seq m)]
      (if (> weight i)
        choice
        (recur (- i weight) remaining)))))
 


(defn grow-random-tree 
  "Grow a max-depth limited random code vector.
   Optional argument :p-leaf, e.g. (grow-random-tree max-depth :p-leaf 0)
   would force a full, max-depth tree.

   Draws from leaves, unary-ops, binary-ops, quaternary-ops."
  [max-depth & {:keys [p-leaf] :or {p-leaf default-p-leaf}}]
  (cond
    (<= max-depth 0) (rand-nth leaves)
    (< (rand) p-leaf) (rand-nth leaves)
    :else
      (let [head (rand-nth (keys ops))
            arity (ops head)
            sub-trees (for [i (range arity)]
                        (grow-random-tree (- max-depth 1) :p-leaf p-leaf))]
        
        (into [head] sub-trees))))

(defn main
  "Examples and stuff"
  [& args]

  (println "; random tree evaluated at [x y z] = [1 2 3]")
  (let [random-tree (grow-random-tree 10)]
    (pprint-code (to-code random-tree))
    (println "=>" ((to-lambda random-tree) 1 2 3))))


