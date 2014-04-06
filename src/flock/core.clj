(ns flock.core)

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

;;;

(defn listify
  "Recursively convert nested collection v into nested list."
  [v]
  (if (coll? v)
    (list* (map listify v))
    v))

(defn to-code
  "Convert nested collection to (unevaluated) anonymous function.
   Suitable for viewing a code-vector.
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

(defn main
  "Examples and stuff"
  [& args]
 
  (prn (to-code params code-vector)) ;; print object
  (println "At x = 1")
  (println ((to-lambda params code-vector) 1))
  )


