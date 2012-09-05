(ns quat-julia.unbounding-raymarch 
  "Unbounding volume ray-marching of the quaternion julia set, see Ray tracing 
   deterministic 3-D fractals by Hart et al." 
  (:use quat-julia.vec-utils))

(defn julia-distance-estimate [q c max-iterations]
  (loop [iter 0 dz [1.0 0 0 0] z q]
    (let [z-length (vec-length z)]     
      (if (and (<= z-length 4) (< iter max-iterations))
        (recur (inc iter) 
               (vec-add (vec-scale 2.0 (quat-mul z dz)) [1.0 0 0 0])
               (vec-add (quat-mul z z) c))
        (/ (* z-length (Math/log z-length)) (vec-length dz))))))

(defn unbounding-ray-march [c max-julia-iterations param max-distance-iterations
                            epsilon min-z vp-x vp-y]
  (loop [iter 0 z min-z dist (+ 1 epsilon)]
    (if (and (< iter max-distance-iterations) (> dist epsilon))
      (let [ dist-estimate (julia-distance-estimate [vp-x vp-y z param] c max-julia-iterations)]
        (recur (inc iter) (+ z dist-estimate) dist-estimate)) z)))
  