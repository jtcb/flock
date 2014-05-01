(ns flock.core
  (:require [clojure.pprint :as pprint]
            [flock.fitness :as f])
  (:import (java.util Random)))

(def r
  "Instance of Java Random object for global use (threadsafe)."
  (Random.))

;; Parameters of the GP system

(def sigma
  "Standard deviation of number generated at the base of gp trees."
  10)

(def maximum-depth
  "Default maximum depth of genotype.

   NOTE
   The Sun JVM has a 64k limit for classfiles. We risk generating functions
   too large with maximum-depth > 8, although this is unlikely. I've seen it
   happen with depth=12, though.
   
   Possible workaround: switch to ternary if. This would guarentee safety with
   depth=10."
  10)

(def default-p-leaf
  "Default probability of a generating a leaf when growing a tree."
  0.5)

(def params 
  "Formal parameters of generated gp functions."
  ['x 'y 'z])

(def k
  "k-selection for tournament"
  2)

(def pm
  "Probability of mutation. P(crossover) = 1 - pm"
  0.05)

(def terminals
  "Terminals of generated gp functions.
   
   TODO: currently, the only type of terminal is some (normally
   distributed) real number, but can be extened."
  [:normal])

(def leaves
  "params U terminals"
  (into params terminals))

;; Special GP functions

(def pair
  "Representation for returning a pair of objects. Currently, aliased to list"
  list)

(defn div
  "Protected division. Division by zero returns 1."
  [x y]
  (if (== y 0)
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
  "Recursively convert nested collection v into nested lists.
   TODO consider changing coll? to vector? for interop concerns"
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


(defn new-genotype
  "Create a fresh (random) genotype. Genotypes consist of a vector of
   [pair code-vector-for-x code-vector-for-y].

   pair acts the head of the genotype and will never be replaced/moved by
   genetic operators. (This also simplifies the interface.)
  
   The x and y fns are generated using force-tree."
  [max-depth & {:keys [p-leaf] :or {p-leaf default-p-leaf}}]
  (conj ['pair] (force-tree (- max-depth 1) :p-leaf p-leaf)
                (force-tree (- max-depth 1) :p-leaf p-leaf)))

(defn crossover-points
  "Return lazy-seq of crossover/mutation points."
  [v]
  (if (vector? v)
    (for [i (range 1 (count v))
          p (conj (crossover-points (v i)) nil)]
      (conj p i))
  ;else
    nil))

(defn mutate
  "Return the result of point mutating genotype g.
   Operation obeys maximum-depth i.e. new genotype will not be too deep,
   assuming that the genotype's depth is already less than maximum-depth."
  [g & {:keys [p-leaf] :or {p-leaf default-p-leaf}}]
  (let [cp (rand-nth (crossover-points g))
        depth (count cp)
        new-subtree (grow-random-tree (- maximum-depth depth) :p-leaf p-leaf)]
    (assoc-in g cp new-subtree)))

(defn vector-depth
  "Return the nesting depth of nested vectors.
   (Not very efficient.)"
  [v]
  (let [cp (crossover-points v)]
    (apply max (map count cp))))

(defn crossover
  "Return the result of crossover between genotypes p1 and p2 as (pair c1 c2)
   
   Obeys maximum depth (assuming both parents do); if result of crossover
   would have produced a genotype too deep, then only the depth appropriate
   child will be returned." 
  [p1 p2]
  (let [p1-crosspoint (rand-nth (crossover-points p1))
        p2-crosspoint (rand-nth (crossover-points p2))
        p1-tail (get-in p1 p1-crosspoint)
        p2-tail (get-in p2 p2-crosspoint)
        c1 (assoc-in p1 p1-crosspoint p2-tail)
        c2 (assoc-in p2 p2-crosspoint p1-tail)]
    (cond
      (> (vector-depth c2) maximum-depth) (pair c1)
      (> (vector-depth c1) maximum-depth) (pair c2)
      :else (pair c1 c2))))

;; Actual GP system

(defn calculate-fitness
  "Wrapper to flock.fitness/fitness

   Note: (to-lambda genotype) will convert to anonymous function of params
  
   TODO: implement"
  [genotype]
  (- (count (flatten genotype))))

(defn roulette
  "Select one of m's keys with probability proportional to its value.
   e.g. (roulette {:a 1 :b 2}) will select :a with probability 1/3.
   
   Values should be non-negative numbers."
  [m]
  (let [total (reduce + (map second m))]
    (loop [i (rand total)
           [[choice weight] & remaining] (seq m)]
      (if (> weight i)
        choice
        (recur (- i weight) remaining)))))


(defn ramped-half-and-half 
  "Ramped half and half method to generate initial population.
   Generates 2 * half-size with half-size many trees generated to ramp-depth
   and the other half generated randomly.
  
   All trees are limited by the globally defined maximum-depth."
  [half-size ramp-depth]
  (let [full (for [i (range 0 half-size)]
               (new-genotype (min ramp-depth maximum-depth) :p-leaf 0))
        random (for [i (range 0 half-size)]
                 (new-genotype maximum-depth))
        population (into [] (concat full random))
        fits (into [] (map #(calculate-fitness %) population))]

    {:genotype population, :fitness fits}))


(defn tournament
  "Out of k randomly selected genotypes, return the one with best fitness."
  [population k]
  (first
    (apply max-key #(second %)
      (for [i (range k)
            :let [r (rand-int (count (:genotype population)))]]
        [(nth (:genotype population) r) (nth (:fitness population) r)]))))


(defn next-generation
  "Generational GP, tournament selection, 1 elite clone"
  [population]
  (let [pop-size (count (:genotype population))
        best-index (.indexOf (:fitness population) 
                              (apply max (:fitness population)))
        elite-genotype (nth (:genotype population) best-index)
        elite-fitness (nth (:fitness population) best-index)
        new-genotypes
          (loop [new-genes (vector)]
            (cond
              (= (count new-genes) (dec pop-size)) ; finished
                new-genes
              (> (count new-genes) (dec pop-size)) ; extra genotype
                (pop new-genes) 
              (< (rand) pm)                        ; mutate
                (let [parent (tournament population k)]
                  (recur (conj new-genes (mutate parent))))
              :else
                (let [p1 (tournament population k)
                      p2 (tournament population k)]
                  (recur (into new-genes (crossover p1 p2))))))
        new-fitness
          (into [] (map #(calculate-fitness %) new-genotypes))]
     
    {:genotype (conj new-genotypes elite-genotype),
     :fitness (conj new-fitness elite-fitness)}))

(defn evolve
  "Return the population after n rounds of evolution."
  [population n]
  (if (<= n 0)
    population 
    (recur (next-generation population) (- n 1))))

(defn -main
  "Executed via $ lein run" 
  [& args]
  (do (print "Running ") (apply print args) (println) (println "---")

  (let [population-size 100
        num-generations 10
        initial-population (ramped-half-and-half population-size 5)
        final-population (evolve initial-population num-generations)
        last-clone (last (:genotype final-population))
        last-fitness (last (:fitness final-population))]
    (println "Fitness:" last-fitness)
    (pprint-code (to-code last-clone))
    (println)
    (pprint-code final-population))))

