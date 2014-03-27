(ns flock.core)

;; dynamically rebindable variables
(def ^:dynamic x 1)
(def ^:dynamic y 2)

;; vectors allow fast access/replacement of interior elements
;; symbols are quoted to hold evaluation
(def code-vector
  ['if ['= 1 ['+ 'x 0]]
       ['println "is 1"]
       ['println "not 1"]])

;; assoc-in updates nested associative collections
(def new-code-vector (assoc-in code-vector [1 2 1] 'y))

(def v-inc
  ['inc 'x])

;;;

(defn listify
  "Recursively convert nested collection v into nested list."
  [v]
  (if (coll? v)
    (list* (map listify v))
    v))

(defn veval
  "Evalulate nested collections as code."
  [v]
  (eval (listify v)))

(defn to-lambda 
  "Convert nested collection to anonymous function.
   Pass params as quoted symbols. e.g. (to-lambda v 'x 'y)"
  [v & params]
  (let [body (list (listify v))
        args (vec params)
        full (conj body args 'fn)] ;; full-function definition
    (eval full)))                  ;; eval creates the actual lambda

(defn main
  "Examples and stuff"
  [& args]
  
  (println "x:" x "y" y)
  (println "code-vector:" code-vector)

  (veval code-vector)
  
  (println "new-code-vector:" new-code-vector)

  (veval new-code-vector)

  (println "v-inc" v-inc)
  (println "evaluating v-inc at 5")
  ((to-lambda v-inc 'x) 1)
  )


