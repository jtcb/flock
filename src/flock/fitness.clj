(def SHEEP 20)
(def WOLVES 1)
(def field [100 100])
(def pen [25 25])
(def dist-fence 5)
(def dist-sheep 10)
(def dist-wolf 15)
(def force-fence 1)
(def force-wolf 1)
(def force-sheep-attract 0.1)
(def force-sheep-repel 0.01)
(def force-pen 0.1)
(def vmax-sheep 1)
(def vmax-wolf (* 3 vmax-sheep))

(defn square [x] (* x x))

(defn normalize-vec
	"Vector v is rescaled (if necessary) to magnitude maxv"
	[maxv, v]
	(let	[mag (reduce #(+ (square %1) (square %2)) v)]
		(if (<= mag (square maxv)) v (map (partial * (/ maxv (Math/sqrt mag))) v))
	)
)

(defn fence-check
	"Returns a new set of coordinates. Result will be different from
	new only if a fence has been penetrated"
	[old new]
	(let [x1 (first old) , y1 (second old) , x2 (first new) , y2 (second new) ,
			x2 (if (< x2 0) 0 x2) ,
			x2 (if (> x2 (field 0)) (field 0) x2) ,
			y2 (if (< y2 0) 0 y2) ,
			y2 (if (> y2 (field 1)) (field 1) y2) ,
			newx (+ (pen 0) (if (< x1 x2) -0.001 0.001)) 
			]
		(if (or (and (< x1 (pen 0)) (>= x2 (pen 0))) (and (> x1 (pen 0)) (<= x2 (pen 0))))
			(let [intercept (+ y1 (/ (* (- (pen 0) x1) (- y2 y1)) (- x2 x1)))]
				(if (<= intercept (pen 1)) (list newx intercept) (list x2 y2)))
			(list x2 y2))
	)
)

(defn calc-sheep-f 
	"Returns the net force acting on sheep with coordinates pos"
	[pos, wolf-pos, sheep-pos]
	(let [x (first pos) , y (second pos) ,
			; Fence forces
			fx (if (< x dist-fence) (* force-fence (- 1 (/ x dist-fence))) 0) ,
			fy (if (< y dist-fence) (* force-fence (- 1 (/ y dist-fence))) 0) ,
			fx (- fx (if (> x (- (field 0) dist-fence)) (* force-fence (- 1 (/ (- (field 0) x) dist-fence))) 0)) ,
			fy (- fy (if (> y (- (field 1) dist-fence)) (* force-fence (- 1 (/ (- (field 1) y) dist-fence))) 0)) ,
			fx (+ fx (if (or (> y (pen 1)) (> x (+ (pen 0) dist-fence)) (< x (- (pen 0) dist-fence))) 
								0 
								(if (>= x (pen 0)) (* force-fence (- 1 (/ (- x (pen 0)) dist-fence)))
														 (* force-fence (- (/ (- (pen 0) x) dist-fence) 1)))))
			; Pen force
			pen-force (if (not-every? identity (map < pos pen)) [0 0]
								(let [pen-vec (map #(- (/ %1 2) %2) pen pos)
										d (Math/sqrt (reduce #(+ (square %1) (square %2)) pen-vec))]
										(map (partial * (/ force-pen d)) pen-vec)
								))
			; Wolf forces
			wolf-vec (map #(map - %1 %2) (repeat pos) wolf-pos)
			wolf-dist (map (partial reduce #(+ (square %1) (square %2))) wolf-vec)
			wolf-force (map 	(fn [d v]
										(if (> d (square dist-wolf)) [0 0]
											(map (partial * (/ (* force-wolf (- (/ (square dist-wolf) d) 1)) (Math/sqrt d))) v))
									)
									wolf-dist 
									wolf-vec)
			wolf-force (reduce #(map + %1 %2) wolf-force)
			; Sheep forces
			sheep-vec (map #(map - %1 %2) sheep-pos (repeat pos))
			sheep-dist (map (partial reduce #(+ (square %1) (square %2))) sheep-vec)
			sheep-force (map 	(fn [d v]
										(if (> d (square dist-sheep)) [0 0]
											(map (partial * (/ (- force-sheep-attract (* force-sheep-repel (/ (square dist-sheep) d))) (Math/sqrt d))) v))
									)
									sheep-dist 
									sheep-vec)
			sheep-force (reduce #(map + %1 %2) sheep-force)			
			]
			(map (partial +) (vector fx fy) pen-force wolf-force sheep-force)
	)
)

(defn dumb-wolf 
	"Generates random wolf force vector"
	[& args]
	(repeatedly WOLVES #(list (- (rand) 0.5) (- (rand) 0.5)))
)

(defn fitness
	"Returns percentage of sheep in pen after simulation has run for tmax steps"
	([wolf-AI] (fitness wolf-AI nil))
	([wolf-AI filename]
		(let	[tmax 1000
				 fout (if filename (clojure.java.io/writer filename) nil)
				]
			(when fout (.write fout (str 	(.toString (field 0)) " 0 0 0 0 " (.toString (field 1)) " " (apply pr-str field) "\r\n"
													(.toString (pen 0)) " 0 0 0 0 " (.toString (pen 1)) " " (apply pr-str pen) "\r\n"	
													(.toString WOLVES) " " (.toString SHEEP) " " (.toString tmax) "\r\n")))
			; Writes initial simulation variables to file if argument filename is non-nil
			
			(loop [t 0
					 wolf-pos (map #(map * % pen) (repeatedly WOLVES #(list (rand) (rand)))) ,
					 wolf-vel (repeat WOLVES '(0 0)) ,
					 sheep-pos (map #(map * % field) (repeatedly SHEEP #(list (+ (rand 0.5) 0.5) (rand)))) ,
					 sheep-vel (map #(map * % [vmax-sheep vmax-sheep]) (repeatedly SHEEP #(list (- (rand) 0.5) (- (rand) 0.5))))
					]
				; Wolves are placed randomly inside pen area, with no velocity
				; Sheep are placed randomly in right half of field with random velocities
								 				
				(if (= t tmax) (do 	(when fout (.close fout)) 
											(/ (count (filter #(every? identity (map < % pen)) sheep-pos)) SHEEP)
									)
									; At end of simulation, return percentage of sheep inside pen
					(let [args '(1 2 3) 
							; modify this to supply postions/velocities to wolf-AI in desired form
							new-wolf-vel  (map #(map + %1 %2) wolf-vel (wolf-AI args)) ,
							; wolf-AI is expected to return a list of wolf forces: ( [F_x,F_y] [F_x,F_y] ...)
							new-wolf-vel (map (partial normalize-vec vmax-wolf) new-wolf-vel) ,
							; rescale velocities which are larger than vmax-wolf
							temp-wolf-pos (map #(map + %1 %2) wolf-pos new-wolf-vel) ,
							; modify wolf positions
							new-wolf-pos (map fence-check wolf-pos temp-wolf-pos) ,
							; check for fence penetrations and resolve
							new-wolf-vel (map (fn [a b c] (map #(if (= %2 %3) %1 0) a b c)) new-wolf-vel temp-wolf-pos new-wolf-pos) ,
							; set penetrating velocities to 0
							sheep-force (map #(calc-sheep-f % wolf-pos (remove #{%} sheep-pos)) sheep-pos) ,
							new-sheep-vel  (map #(map + %1 %2) sheep-vel sheep-force) ,
							; calc sheep forces and new velocities
							new-sheep-vel (map (partial normalize-vec vmax-sheep) new-sheep-vel) ,
							; rescale velocities larger than vmax-sheep							
							temp-sheep-pos (map #(map + %1 %2) sheep-pos new-sheep-vel) ,
							; modify sheep positions
							new-sheep-pos (map fence-check sheep-pos temp-sheep-pos) ,
							; check for fence penetrations and resolve
							new-sheep-vel (map (fn [a b c] (map #(if (= %2 %3) %1 0) a b c)) new-sheep-vel temp-sheep-pos new-sheep-pos) ,
							; set penetrating velocities to 0
							]
						(when fout (.write fout (str (apply pr-str (flatten wolf-pos)) " " (apply pr-str (flatten sheep-pos)) "\r\n")))
						(recur	(inc t)
									new-wolf-pos
									new-wolf-vel
									new-sheep-pos
									new-sheep-vel
						)
					)
				)
			)
		)
	)
)