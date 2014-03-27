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

(def blah
  ['inc 'x])

(defn listify
  "Convert nested collection v into nested list."
  [v]
  (if (coll? v)
    (list* (map listify v))
    v))

(defn veval
  "Evalulate nested vectors as code."
  [v]
  (eval (listify v)))

(defn main
  "Examples and stuff"
  [& args]
  
  (println "x:" x "y" y)
  (println "code-vector:" code-vector)

  (veval code-vector)
  
  (println "new-code-vector:" new-code-vector)

  (veval new-code-vector)
 
  ;; dynamically rebind x to 10, evaluate blah in this context
  ;; restores original value of x afterwards
  (binding [x 10] (veval blah))
  )


