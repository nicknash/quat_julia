(ns quat-julia.naive-raymarch
  "Very naive ray-marching for the quaternion julia set"
  (:use quat-julia.vec-utils))

(defn outside-julia? [q c max-iterations] 
  (loop [iter 0 z q]
    (if (> (vec-length z) 4) 
      true 
      (if (< iter max-iterations) 
        (recur (inc iter) (vec-add (quat-mul z z) c)) 
        false))))

(defn ray-march-z [keep-marching? get-next-z initial-z min-z max-z num-steps]
  (let [dz (float (/ (- max-z min-z) num-steps))]
    (loop [z initial-z]  
      (if (keep-marching? z) 
        (recur (get-next-z z dz)) z)))) 

(defn ray-march-forward-then-backward [outside? min-z max-z
                 num-coarse-steps num-fine-steps] 
  (let [z (ray-march-z #(and (outside? %) (< % max-z)) 
                       #(+ %1 %2) 
                       min-z min-z max-z num-coarse-steps)]
    (if (< z max-z) (ray-march-z #(and (not (outside? %)) (> % min-z))
                                 #(- %1 %2)
                                 z min-z max-z num-fine-steps) max-z)))

(defn ray-march-julia [c max-iterations param 
                       num-coarse-steps num-fine-steps 
                       min-z max-z x y]
  (letfn [(outside? [z] (outside-julia? [x y z param] c max-iterations))]
    (ray-march-forward-then-backward 
      outside? min-z max-z num-coarse-steps num-fine-steps)))
