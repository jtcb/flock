(ns flock.core
  (:require [clojure.pprint :as pprint])
  (:import (java.util Random)))

(def r
  "Instance of Java Random object for global use (threadsafe)."
  (Random.))

;; Parameters of the GP system

(def sigma
  "Standard deviation of number generated at the base of gp trees."
  10)

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
  "Terminals of generated gp functions.
   
   TODO: currently, the only type of terminal is some (normally
   distributed) real number, but can be extened.
   Do we want actual numbers to be as likely as a formal parameter?"
  (into []
        (for [i (range (count params))] :normal)))

(def leaves
  "params U terminals (do not modify)"
  (into params terminals))

;; Special GP functions

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

(def ops
  "Map of: operations that can appear in a generated gp code => their arities
   TODO: Add more ops? (e.g. max/min, average)"
  {'*-1 1, '+ 2, '- 2, '* 2 'div 2, 'qif 4})

;;
;; utility functions
;;

(defn rand-normal
  "Normally distributed random number."
  [mu sigma]
  (-> r .nextGaussian (* sigma) (+ mu)))

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
   
   Leaf nodes are drawn from leaves, interior nodes from ops."
  [max-depth & {:keys [p-leaf] :or {p-leaf default-p-leaf}}]
  (cond
    (or (<= max-depth 0)
        (< (rand) p-leaf))
      (let [leaf (rand-nth leaves)]
        (if (= leaf :normal)
          (rand-normal 0 sigma)
          leaf))
    :else
      (let [head (rand-nth (keys ops))
            arity (ops head)
            sub-trees (for [i (range arity)]
                        (grow-random-tree (- max-depth 1) :p-leaf p-leaf))]
        
        (into [head] sub-trees))))


(defn force-tree
  "grow-random-tree with p-leaf will result in p-leaf percent of the generated
   functions being of depth 0 (e.g. (fn [params] some-constant)) This may be
   undesirable for larger p-leaf values.
   
   force-tree forces the root of the generated tree to be a function,
   guarenteeing some minimum branching, even with larger p-leaf values."
  [max-depth & {:keys [p-leaf] :or {p-leaf default-p-leaf}}]
  (let [head (rand-nth (keys ops))
        arity (ops head)
        sub-trees (for [i (range arity)]
                    (grow-random-tree (- max-depth 1) :p-leaf p-leaf))]
        (into [head] sub-trees)))


;; TODO mutate sections of a tree (pick item, replace w/ new random tree)
;;      conduct crossover
;;      generate an initial popuation, eval fitness, etc...

(defn main
  "Examples and stuff"
  [& args]

  (println "; random tree evaluated at [x y z] = [1 2 3]")
  (let [some-tree (force-tree 10)]
    (pprint-code (to-code some-tree))
    (println "=>" ((to-lambda some-tree) 1 2 3))))


