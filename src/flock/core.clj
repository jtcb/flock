(ns flock.core)

;;; USED FOR QUICK TESTING; TODO: remove

;; vectors allow fast access/replacement of interior elements
;; symbols are quoted to hold evaluation (otherwise, they'd resolve)
(def code-vector
  ['if ['= 1 ['+ 'x 0]]
       ['println "is 1"]
       ['println "not 1"]])

;; assoc-in updates nested associative collections
(def new-code-vector (assoc-in code-vector [1 2 1] 'y))

(def v-inc
  ['inc 'x])

(def params ['x])

;;; END QUICK TESTING


;; gp infrastucture

(defn listify
  "Recursively convert nested collection v into nested list."
  [v]
  (if (coll? v)
    (list* (map listify v))
    v))

(defn to-code
  "Convert nested collection to (unevaluated) anonymous function.
   Suitable for viewing a code-vector via (prn ...)
   Pass params as vector of quoted symbols. e.g. (to-code ['x 'y] v)"
  [params v]
  (let [body (list (listify v))
        full (conj body params 'fn)]
    full)) 

(defn to-lambda 
  "Convert nested collection to anonymous function.
   Pass params as vector of quoted symbols. e.g. (to-lambda ['x 'y] v)"
  [params v]
  (eval (to-code params v)))


;; things that go in gp functions

(def p-leaf "Probability of a generating a leaf when growing a tree."
  0.5)

(def params ['x 'y 'z])

(def terminals [0 1 2])

;; leaves of a tree
(def leaves (into [] (concat params terminals)))

(defn div
  "Protected division. Division by zero returns 1."
  [x y]
  (if (= y 0)
    1
    (/ x y)))

;; not sure if we *want* unary ops, but the option is here in case.
(def unary-ops '[-])

(def binary-ops '[+ - * div])

;; (if) is the only ternary op I can think we might want. However, we may want
;; to define it as a 4-ary operation if-a<=b-c-else-d, so we can stick to pure
;; numbers. Thoughts?


(defn grow-random-tree 
  "Grow a max-depth limited random code vector.
   Setting p-leaf to 0 is equivalent to building a full, max-depth tree.

   TODO: add unary/ternary/4-ary op support."
  [max-depth]
  (cond
    (<= max-depth 0) (rand-nth leaves)
    (< (rand) p-leaf) (rand-nth leaves)
    :else
      (let [head (rand-nth binary-ops)
            arg1 (grow-random-tree (- max-depth 1))
            arg2 (grow-random-tree (- max-depth 1))]
        [head arg1 arg2])))


(defn main
  "Examples and stuff"
  [& args]

  (println "random tree; evaluated at [x y z] = [1 2 3]")
  (let [random-tree (grow-random-tree 10)]
    (prn (to-code params random-tree))
    (println ((to-lambda params random-tree) 1 2 3)))
  )


