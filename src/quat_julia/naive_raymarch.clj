(ns quat-julia.naive-raymarch
  "Very naive ray-marching for the quaternion julia set"
  (:use quat-julia.vec-utils))


; Julia set

(defn outside-julia? [q c max-iterations] 
  (loop [iter 0 z q]
    (if (> (vec-length z) 4) 
      true 
      (if (< iter max-iterations) 
        (recur (inc iter) (vec-add (quat-mul z z) c)) 
        false))))

; Naive Ray marching

(defn ray-march-forward [outside? initial-z max-z num-steps]
  (let [dz (float (/ (- max-z initial-z) num-steps))]
    (loop [z initial-z]  
      (if (and (outside? z) (< z max-z)) 
        (recur (+ z dz)) z)))) 

(defn ray-march-backward [inside? initial-z min-z max-z num-steps]
  (let [dz (float (/ (- max-z min-z) num-steps))]
    (loop [z initial-z]
      (if (and (inside? z) (> z min-z)) 
        (recur (- z dz)) z))))

(defn ray-march [outside? min-z max-z
                 num-coarse-steps num-fine-steps] 
  (let [z (ray-march-forward outside? min-z max-z num-coarse-steps)]
    (if (< z max-z) (ray-march-backward #(not (outside? %)) z min-z max-z num-fine-steps) max-z)))

(defn ray-march-julia [c max-iterations param 
                       num-coarse-steps num-fine-steps 
                       min-z max-z x y]
  (letfn [(outside? [z] (outside-julia? [x y z param] c max-iterations))]
    (ray-march outside? min-z max-z num-coarse-steps num-fine-steps)))
