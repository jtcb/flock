
(defn mean [coll] (/ (reduce + coll) (count coll)))

(defn variance [coll]
  (let [avg (mean coll)]
    (/ (reduce + (for [x coll] (* (- x avg) (- x avg))))
       (dec (count coll)))))
       
(defn split-wolf [wx wy nfsx nfsy]
  (pair
    (qif
      (-
        (+ (* wx nfsx) (* nfsx 7.636755100377311))
        (div (*-1 nfsx) (- nfsy 5.227143040591859)))
      (div
        (div
          (+
            -9.655274978504828
            (+ (div wy nfsx) (qif 8.798063425913643 wy nfsx nfsx)))
          (* nfsx -1.79340421337064))
        (- (+ nfsx 11.774191693589906) (+ 4.823388776038026 wx)))
      (qif
        (+
          (qif
            14.237266402985757
            nfsx
            (+ (* nfsx 6.77126278745952) (div 1.2787697508452178 wy))
            10.966740210828494)
          (*-1 nfsx))
        (qif
          (* nfsy nfsy)
          (div nfsy nfsy)
          (+ -16.022840725110914 nfsy)
          nfsy)
        (*-1 (*-1 nfsx))
        (*-1 (+ nfsy nfsy)))
      (*
        (* (qif nfsx nfsx wy wy) (- nfsy wx))
        (* (*-1 nfsx) (- wx nfsy))))
    (div
      (div
        (+ (* nfsx nfsy) (* wx nfsy))
        (div (div wx wy) (+ 4.791472610423231 nfsx)))
      (+
        (div
          (qif 0.6929373407271056 wx 8.317899985589323 nfsx)
          (qif wy nfsx nfsy nfsy))
        (+ (qif wx nfsx 25.468726482265502 nfsx) (+ nfsy wx))))))
        
(defn best-wolf [x y nfsx nfsy]
  (pair
    (-
      (-
        (+
          x
          (-
            (* nfsy nfsy)
            (+
              (qif
                11.877382339556322
                x
                (* 7.166344334459449 x)
                21.927393095339916)
              -8.36851351315274)))
        -10.017531412427893)
      (qif
        (- y -0.3632076157900198)
        nfsx
        (-
          (qif
            (- nfsx 21.927393095339916)
            -1.3467936872815565
            x
            (*
              nfsx
              (-
                nfsx
                (-
                  (+ x (* nfsy 2.371770876205635))
                  (- (+ y -10.017531412427893) x)))))
          (*
            nfsy
            (+
              nfsx
              (qif
                x
                (* x nfsy)
                (qif
                  nfsx
                  (* -2.341376928380047 nfsy)
                  (* -7.852427160971881 y)
                  nfsx
)
                nfsx))))
        (* x (+ 5.398782757048797 x))))
    (qif
      21.927393095339916
      (qif
        (*
          (qif nfsy x nfsy (- y (+ (* -1.3467936872815565 x) x)))
          (+
            (qif
              nfsy
              7.166344334459449
              (+
                (+ -0.2159774695266377 x)
                (+ (* nfsx nfsy) (* nfsy y)))
              (* y 5.398782757048797))
            11.877382339556322))
        -12.411594130071359
        (*
          (qif
            (qif
              (* nfsx (* nfsy nfsx))
              (+ x 2.302448602409197)
              (qif
                nfsy
                nfsy
                (qif
                  (+ (qif 19.398748795625362 nfsx nfsx y) x)
                  2.2675869705663803
                  (* x (+ 5.398782757048797 x))
                  (- -6.791533705025077 y))
                x)
              (+
                (qif
                  11.877382339556322
                  5.398782757048797

                  x
                  21.927393095339916)
                -8.36851351315274))
            y
            -8.249118213602832
            (+ x (* nfsy 2.371770876205635)))
          (-
            (- y y)
            (-
              5.917289123088096
              (qif
                (qif
                  (qif
                    (+ 5.398782757048797 x)
                    (* x nfsy)
                    (+ 5.398782757048797 x)
                    -0.3632076157900198)
                  nfsy
                  nfsx
                  nfsy)
                (- nfsy y)
                (- 10.503058370947713 -25.07058022713052)
                (- nfsx nfsy)))))
        (+
          (+
            nfsy
            (-
              (*
                (- nfsy y)
                (+ nfsy (* -1.3467936872815565 (+ x nfsy))))
              y))
          x))
      -8.249118213602832
      (* x nfsy))))
      
        
    