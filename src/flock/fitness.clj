(ns flock.fitness
  (:import (java.util Arrays)))

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

; Simulation parameters
; WOLVES must be >= 0
(def WOLVES 1)
; SHEEP must be > 0
(def SHEEP 20)
; Field/Pen [width, height].  Both have upper left corner located at (0,0)
(def field [100 100])
(def pen [25 25])
; If creature penetrates a fence, the penetrating coordinate is randomly
; reset to within fence-spawn distance of the fence.  This is done to
; prevent sheep-stacking in the corners
(def fence-spawn 0.2)

; Force ranges
(def dist-fence 5)
(def dist-sheep 20)
(def dist-wolf 30)
; Force magnitudes
(def force-fence 1)
(def force-wolf 5)
(def force-sheep-attract 0.1)
(def force-sheep-repel 0.05)
(def force-pen 0.1)
; Maximum velocities
(def vmax-sheep 1)
(def vmax-wolf (* 3 vmax-sheep))

; Useful precalculations
(def fieldx (field 0))
(def fieldy (field 1))
(def penx (pen 0))
(def peny (pen 1))
(def fieldxb (- fieldx dist-fence))
(def fieldyb (- fieldy dist-fence))
(def penxbl (- penx dist-fence))
(def penxbr (+ penx dist-fence))
(def fratio (/ force-fence dist-fence))
(def pencenterx (/ (pen 0) 2))
(def pencentery (/ (pen 1) 2))
(def dist-wolf2 (* dist-wolf dist-wolf))
(def dist-sheep2 (* dist-sheep dist-sheep))
(def vmax-wolf2 (* vmax-wolf vmax-wolf))
(def vmax-sheep2 (* vmax-sheep vmax-sheep))

(defmacro pr-arr
	"Converts primitive array to a string without the
	annoying formatting of toString"
	[x]
	`(.replace
		(.replace
			(.replace (Arrays/toString ~x)
				"[" "")
			"]" "")
		"," ""))

(defmacro sqr [x] `(* ~x ~x))

(defmacro ainc
	"arr[idx] += val"
	[arr idx val]
	`(aset ~arr ~idx (+ (aget ~arr ~idx) ~val))
)

(defmacro forloop
	"For when you get tired of writing loop and recur everywhere"
	[idx start end increment & body]
	`(loop [~idx ~start]
		(when (< ~idx ~end)
			~@body
			(recur (+ ~idx ~increment))))
)

(defn initialize-rand-arr
	"Returns an array with (size) pairs of random elements, the first of
	which	ranges from (start1) to (start1+range1), the second of which ranges
	from (start2) to (start2+range2)"
	^doubles [size start1 range1 start2 range2]
	(let [size (* 2 size)
			arr (double-array size)]
		(forloop i 0 size 2
			(aset arr i (-> (Math/random) (* range1) (+ start1)))
			(aset arr (inc i) (-> (Math/random) (* range2) (+ start2)))
		)
		arr
	)
)

(defn eval-fitness
	"Called at end of simulation, returns fraction of sheep in pen"
	[^doubles wolf-pos ^doubles sheep-pos]
	(loop [i 0 count 0]
		(if (< i (alength sheep-pos))
			(recur	(+ 2 i)
						(if (and (= (aget sheep-pos i) 0.0) (= (aget sheep-pos (inc i)) 0.0))
							(inc count)
							count))
			(/ count SHEEP))))

(defn dumb-wolf
	"Returns a pair of random values"
	[& args]
	(list (- (Math/random) 0.5) (- (Math/random) 0.5))
)

(defn calc-sheep-f
	"Calculates force on each sheep and writes result to (f) array"
	[^doubles wolf-pos ^doubles sheep-pos ^doubles f]
	(forloop i 0 (alength sheep-pos) 2
		(let [j (inc i)
				x (aget sheep-pos i)
				y (aget sheep-pos j)]
				(aset f i 0.0)
				(aset f j 0.0)

				; Fence forces - If sheep is within dist-fence of a fence
				; then it experiences a force which grows linearly from 0
				; at dist-fence away, to force-fence at the fence

				; Left fence force
				(if (< x dist-fence)
					(ainc f i (-> dist-fence (- x) (* fratio))))
				; Right fence force
				(if (> x fieldxb)
					(ainc f i (-> fieldxb (- x) (* fratio))))
				; Upper fence force
				(if (< y dist-fence)
					(ainc f j (-> dist-fence (- y) (* fratio))))
				; Lower fence force
				(if (> y fieldyb)
					(ainc f j (-> fieldyb (- y) (* fratio))))
				; Pen fence force
				(if-not (or	(>= y peny)	(<= x penxbl) (>= x penxbr))
					(ainc f i (-> (if (< x penx) penxbl penxbr)
										(- x) (* fratio))))

				; Wolf force - If sheep is within dist-wolf of a wolf, it
				; experiences a 1/r^2 repulsive force whose strength scales
				; with force-wolf.  This is offset to be 0 at dist-wolf.

				(forloop k 0 (alength wolf-pos) 2
					(let [wx (- x (aget wolf-pos k))
							wy (- y (aget wolf-pos (inc k)))
							d2 (+ (sqr wx) (sqr wy))]
						(if (and (< d2 dist-wolf2) (not= d2 0.0))
							(let [mag (->	dist-wolf2
												(/ d2)
												(- 1)
												(* force-wolf)
												(/ (Math/sqrt d2)))]
								(ainc f i (* mag wx))
								(ainc f j (* mag wy))))))

				; Sheep force - If sheep is within dist-sheep of another sheep,
				; it experiences 2 forces: a constant attractive force with
				; magnitude force-sheep-attract, as well as a repulsive 1/r^2
				; force whose strength is set by force-sheep-repel.
				; The repulsive force is offset to be 0 at dist-sheep.

				(forloop k 0 (alength sheep-pos) 2
					(let [sx (- x (aget sheep-pos k))
							sy (- y (aget sheep-pos (inc k)))
							d2 (+ (sqr sx) (sqr sy))]
						(if (and (< d2 dist-sheep2) (not= d2 0.0))
							(let [mag (->	dist-sheep2
												(/ d2)
												(- 1)
												(* force-sheep-repel)
												(- force-sheep-attract)
												(/ (Math/sqrt d2)))]
								(ainc f i (* mag sx))
								(ainc f j (* mag sy))))))
		)
	)
)

(defn updatev
	"Adds f to v, then checks that the resultant velocities
	do not exceed maxv.  If so, velocities are rescaled"
	[^doubles v ^doubles f ^double maxv ^double maxv2]
	(loop [i 0]
		(if (< i (alength v))
			(let [j (inc i)
					newvx (+ (aget v i) (aget f i))
					newvy (+ (aget v j) (aget f j))
					mag2 (+ (sqr newvx) (sqr newvy))]
				(if (> mag2 maxv2)
					(let [ratio (/ maxv (Math/sqrt mag2))]
						(aset v i (* ratio newvx))
						(aset v j (* ratio newvy)))
					(do
						(aset v i newvx)
						(aset v j newvy)))
				(recur (+ i 2))
			))))

(defn updatepos
	"Adds vel to pos, then checks/resolves fence penetrations"
	[^doubles pos ^doubles vel]
	(forloop i 0 (alength pos) 2
		(let [j	(inc i)
				x1 (aget pos i)
				y1 (aget pos j)
				x2 (+ x1 (aget vel i))
				y2 (+ y1 (aget vel j))
				maxx (if (> x1 x2) x1 x2)
				minx (if (<= x1 x2) x1 x2)
				; Check for pen fence penetration
				x2 (if (or (>= minx penx) (<= maxx penx))
						x2
						(let [intercept (-> (- penx x1) (* (- y2 y1)) (/ (- x2 x1)) (+ y1))]
							(if (>= intercept peny)
								x2
								(let [ratio (if (< x1 x2) (- fence-spawn) fence-spawn)]
									(aset vel i 0.0)
									(+ penx (* ratio (Math/random)))))))
				; Left fence
				x2 (if (< x2 0)
						(do
							(aset vel i 0.0)
							(* fence-spawn (Math/random)))
						x2)
				; Right fence
				x2 (if (> x2 fieldx)
						(do
							(aset vel i 0.0)
							(- fieldx (* fence-spawn (Math/random))))
						x2)
				; Upper fence
				y2 (if (< y2 0)
						(do
							(aset vel j 0.0)
							(* fence-spawn (Math/random)))
						y2)
				; Right fence
				y2 (if (> y2 fieldy)
						(do
							(aset vel j 0.0)
							(- fieldy (* fence-spawn (Math/random))))
						y2)]
			(aset pos i x2)
			(aset pos j y2)
		)
	)
)

(defn calc-wolf-f
	"Calls wolf-AI for each wolf and puts result in (f) array"
	[wolf-AI args ^doubles f]
	; Clojure only lets you type-hint up to 4 function parameters, so this
	; horrible unpackaging of args is unfortunately necessary ...
	(let [wolf-pos ^doubles (args 0)
			wolf-vel ^doubles (args 1)
			sheep-pos ^doubles (args 2)
			sheep-vel ^doubles (args 3)
			
			; Put global calculations here:
;			sheep-x	(loop [i 0 sum 0.0]
;							(if (< i (alength sheep-pos))
;								(recur (+ i 2) (+ sum (aget sheep-pos i)))
;								(/ sum SHEEP)))
;			sheep-y	(loop [i 1 sum 0.0]
;							(if (< i (alength sheep-pos))
;								(recur (+ i 2) (+ sum (aget sheep-pos i)))
;								(/ sum SHEEP)))
			]
		(forloop i 0 (alength f) 2
			(let [x	(aget wolf-pos i)
					y	(aget wolf-pos (inc i))
					
					; Put wolf-specific calculations here:
					
					; Index of closest sheep not in pen
					; Clojure is the worst ...
					nfs	(loop [j 0 
									minj -1
									mindist 0.0]
								(if (< j (alength sheep-pos))	
									(if	(or	(>= (aget sheep-pos j) penx) 
													(>= (aget sheep-pos (inc j)) peny))
											(let [dist (+	(sqr (- (aget sheep-pos j) x))
																(sqr (- (aget sheep-pos (inc j)) y)))]
												(if (and (> dist mindist)
															(not= minj -1))
													(recur (+ j 2) minj mindist)
													(recur (+ j 2) j dist)))
											(recur (+ j 2) minj mindist))
									minj))
					nearest-free-sheep-x	(if (= nfs -1)
													(double fieldx)
													(aget sheep-pos nfs))
					nearest-free-sheep-y (if (= nfs -1)
													(double fieldy)
													(aget sheep-pos (inc nfs)))
													
					; Put all parameters (other than wolf positions) in param list				
					params	(list nearest-free-sheep-x nearest-free-sheep-y)
					; Add other wolf positions to front of param list
					params	(loop [j 0 lst params]
									(if (< j (alength f))
										(if (= i j)
											(recur (+ j 2) lst)
											(recur (+ j 2) (list* 	(aget wolf-pos j) 
																			(aget wolf-pos (inc j)) 
																			lst)))
										lst))
					; Add own position to front of list
					params	(list* x y params)				
					force (apply wolf-AI params)]
				(aset f i ^double (first force))
				(aset f (inc i) ^double (second force))
			)
		)
	)
)

(defn get-in-the-pen
	[^doubles sheep-pos ^doubles sheep-vel]
	(forloop i 0 (alength sheep-pos) 2
		(let [j (inc i)]
			(when (and (< (aget sheep-pos i) penx)
						(< (aget sheep-pos j) peny))
				(aset sheep-pos i 0.0)
				(aset sheep-pos j 0.0)
				(aset sheep-vel i 0.0)
				(aset sheep-vel j 0.0)))))
				
(defn fitness
	"Returns percentage of sheep in pen after simulation has run for tmax steps"
	([wolf-AI] (fitness wolf-AI nil))
	([wolf-AI filename]
		(let [tmax 500
				fout (if filename (clojure.java.io/writer filename) nil)

				wolf-pos (initialize-rand-arr	WOLVES 0 penx 0 peny)
				wolf-vel (double-array (* 2 WOLVES) 0.0)
				sheep-pos (initialize-rand-arr SHEEP (* 0.5 fieldx) (* 0.5 fieldx) 0 fieldy)
				sheep-vel (initialize-rand-arr SHEEP 	(* -0.5 vmax-sheep) vmax-sheep
																	(* -0.5 vmax-sheep) vmax-sheep)
				wolf-force (double-array (* 2 WOLVES))
				sheep-force (double-array (* 2 SHEEP))

				args [wolf-pos wolf-vel sheep-pos sheep-vel]
				]
			; Suck it, immutable data structures!

			(if fout
				(.write fout
					(str	(.toString fieldx) " 0 0 0 0 "
							(.toString fieldy) " " (apply pr-str field) "\r\n"
							(.toString penx) " 0 0 0 0 " (.toString peny) " "
							(apply pr-str pen) "\r\n" (.toString WOLVES) " "
							(.toString SHEEP) " " (.toString (inc tmax)) "\r\n"
							(pr-arr wolf-pos) " " (pr-arr sheep-pos) "\r\n")))

			(loop [t 1]
				(calc-wolf-f wolf-AI args wolf-force)
				(calc-sheep-f wolf-pos sheep-pos sheep-force)
				(updatev wolf-vel wolf-force vmax-wolf vmax-wolf2)
				(updatev sheep-vel sheep-force vmax-sheep vmax-sheep2)
				(updatepos wolf-pos wolf-vel)
				(updatepos sheep-pos sheep-vel)
				(get-in-the-pen sheep-pos sheep-vel)
				(if fout
					(.write fout (str (pr-arr wolf-pos) " " (pr-arr sheep-pos) "\r\n")))
				(if (< t tmax) (recur (inc t)))
			)
			(if fout
				(.close fout))
			(eval-fitness wolf-pos sheep-pos)
		)
	)
)

(defn loopfitness
	[wolf-AI n]
	(loop [i 0 sum 0.0]
		(if (< i n)
			(recur (inc i) (+ sum (fitness wolf-AI)))
			(/ sum n))))