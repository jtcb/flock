(def SHEEP 20)
(def WOLVES 1)
(def field [100 100])
(def pen [25 25])
(def fence-spawn 0.5)
		
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

(defmacro sqr [x] (list '* x x))

(defmacro nestedmap
	"Applies f to collections of collections"
	[f & args]
	(list* 'map `(partial map ~f) args) 
)

(defn sqrmag 
	"Calculates the magnitude squared of vector v"
	[v] 
	(reduce #(+ %1 (* %2 %2))
				0
				v)
)
	
(defn rescale-vec
	"Vector v is rescaled (if necessary) to magnitude maxv"
	[maxv, v]
	(let	[mag2	(sqrmag v)]
		(if 	(<= mag2 (sqr maxv)) 
				v
				(map (partial * (/ maxv (Math/sqrt mag2))) v)
		)
	)
)

(defn fence-check
	"Returns a new set of coordinates. Result will be different from
	new only if a fence has been penetrated."
	[old new]
	(let [x1	(first old)
			x2 (first new)
			minx	(min x1 x2)
			maxx	(max x1 x2)
			; Check if pen fence has been penetrated
			new	(if (or	(>= minx (pen 0))
								(<= maxx (pen 0)))
							new
							(let [y1	(second old)
									y2 (second new)
									intercept	(-> (- (pen 0) x1) (* (- y2 y1)) (/ (- x2 x1)) (+ y1))
									op	(if (< x1 x2) - +)]
								(if (> intercept (pen 1))
										new
										[(op (pen 0) (rand fence-spawn)) y2]
								)
							)
					)
			; Check if field boundaries have been penetrated
			new (map #(if %1 (rand fence-spawn) %2) 
						(map < new [0 0]) 
						new)
			new (map #(if %1 (- %2 (rand fence-spawn)) %3) 
						(map > new field) 
						field 
						new)
			]
			new
	)
)

(defn calc-sheep-f
	"Returns the net force acting on sheep with coordinates pos"
	[pos, wolf-pos, sheep-pos]
	(let [x	(first pos)
			y	(second pos)
			; Fence forces
			fence-boundary	[dist-fence dist-fence]
			fence-force1	(map #(if %1 	0
													(* force-fence (- 1 (/ %2 dist-fence))))
										(map >= pos fence-boundary)
										pos)
			fence-boundary (map - field fence-boundary)
			fence-force2	(map #(if %1	0
													(* force-fence (- (/ %2 dist-fence) 1)))
										(map <= pos fence-boundary)
										(map - field pos))
			fence-force3	(if (or	(> y (pen 1))
											(<= x (- (pen 0) dist-fence))
											(>= x (+ (pen 0) dist-fence)))
										[0 0]
										(if (>= x (pen 0))
												[(* force-fence (- 1 (/ (- x (pen 0)) dist-fence))) 0]
												[(* force-fence (- (/ (- (pen 0) x) dist-fence) 1)) 0]
										)
								)
			; Pen force
			pen-force	(if (not-every? identity (map < pos pen))
									[0 0]
									(let [pen-vec (map #(- (/ %1 2) %2)
																pen
																pos)
											ratio (/ force-pen (Math/sqrt (sqrmag pen-vec)))]
											(map (partial * ratio) pen-vec)
									)
							)
			; Wolf forces
			wolf-vec	(nestedmap - (repeat pos) wolf-pos)
			wolf-dist2	(map sqrmag wolf-vec)
			wolf-force	(map	(fn [d2 v]
										(if (or 	(>= d2 (sqr dist-wolf))
													(= d2 0.0))
												[0 0]
												(map (partial *	(/ force-wolf (Math/sqrt d2))
																		(- (/ (sqr dist-wolf) d2) 1))
														v)
										)
									)
									wolf-dist2
									wolf-vec
							)
			wolf-force	(reduce #(map + %1 %2) wolf-force)
			; Sheep forces
			sheep-vec	(nestedmap - (repeat pos) sheep-pos)
			sheep-dist2	(map sqrmag sheep-vec)
			sheep-force	(map	(fn [d2 v]
										(if (or	(>= d2 (sqr dist-sheep))
													(= d2 0.0))
												[0 0]
												(map #(-> 	(sqr dist-sheep)
																(/ d2)
																(- 1)
																(* force-sheep-repel)
																(- force-sheep-attract)
																(/ (Math/sqrt d2))
																(* %))
														v)
										)
									)
									sheep-dist2
									sheep-vec
							)
			sheep-force (reduce #(map + %1 %2) sheep-force)
			]
			(map + fence-force1 fence-force2 fence-force3 pen-force wolf-force sheep-force)
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
			; Writes initial simulation variables to file if argument filename is non-nil
			(if fout 
				(.write fout 
					(str	(.toString (field 0)) " 0 0 0 0 " 
							(.toString (field 1)) " " (apply pr-str field) "\r\n"
							(.toString (pen 0)) " 0 0 0 0 " (.toString (pen 1)) " " 
							(apply pr-str pen) "\r\n" (.toString WOLVES) " " 
							(.toString SHEEP) " " (.toString tmax) "\r\n"
					)
				)
			)
			
			(loop	[t 1
					 ; Wolves are placed randomly inside pen area, with no velocity
					 wolf-pos	(nestedmap * 	(repeat WOLVES pen) 
														(repeatedly #(list (rand) (rand)))) 
					 wolf-vel	(repeat WOLVES [0 0])
					 ; Sheep are placed randomly in right half of field with random velocities
					 sheep-pos	(nestedmap *	(repeat SHEEP field)
														(repeatedly #(list (+ (rand 0.5) 0.5) (rand))))
					 sheep-vel	(nestedmap *	(repeat SHEEP [vmax-sheep vmax-sheep])
														(repeatedly #(list (- (rand) 0.5) (- (rand) 0.5))))
					]
							 				
				(if (= t tmax)
					; Simulation is over!
					(do 	(when fout 
								(.write fout (str (apply pr-str (flatten wolf-pos)) 
														" " 
														(apply pr-str (flatten sheep-pos)) 
														"\r\n"))
								(.close fout))
							; Return percentage of sheep inside pen
							(/ (count (filter #(every? identity (map < % pen)) sheep-pos)) SHEEP)
					)
					; Keep going				
					(let [; values to be passed to the GAs should be placed here
							args '(1 2 3) 
							
							; wolf-AI is expected to return a list of wolf forces:
							; ( [F_x F_y] [F_x F_y] ...)
							wolf-force	(wolf-AI args)
							sheep-force	(map #(calc-sheep-f % wolf-pos sheep-pos) sheep-pos)
							
							new-wolf-vel	(nestedmap + wolf-vel wolf-force)
							new-sheep-vel	(nestedmap + sheep-vel sheep-force)
							
							; rescale velocities larger than maximum
							new-wolf-vel	(map (partial rescale-vec vmax-wolf) new-wolf-vel)
							new-sheep-vel	(map (partial rescale-vec vmax-sheep) new-sheep-vel)
							
							temp-wolf-pos	(nestedmap + wolf-pos new-wolf-vel)
							temp-sheep-pos	(nestedmap + sheep-pos new-sheep-vel)
							
							; check for fence penetrations and resolve
							new-wolf-pos	(map fence-check wolf-pos temp-wolf-pos)
							new-sheep-pos	(map fence-check sheep-pos temp-sheep-pos)
							
							; set penetrating velocities to 0
							new-wolf-vel 	(nestedmap #(if (= %2 %3) %1 0)
																new-wolf-vel
																temp-wolf-pos
																new-wolf-pos)
							new-sheep-vel	(nestedmap #(if (= %2 %3) %1 0)
																new-sheep-vel
																temp-sheep-pos
																new-sheep-pos)
							]
						; output wolf and sheep positions
						(if fout 
							(.write fout	
								(str 	(apply pr-str (flatten wolf-pos)) " " 
										(apply pr-str (flatten sheep-pos)) "\r\n"
								)
							)
						)
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