(ns quat-julia.vec-utils
  "Minimal utilities for manipulating vectors and quaternions")

(defn vec-add [& args] (vec (apply map + args)))
(defn vec-sub [& args] (vec (apply map - args)))
(defn vec-dot [u v] (reduce + (map * u v)))
(defn vec-length [v] (Math/sqrt (vec-dot v v)))
(defn vec-scale [r v] (vec (map #(* r %) v)))
(defn vec-normalize [v] (vec-scale (/ 1 (vec-length v)) v))
(defn det-2x2 [a b c d] (- (* a d) (* b c)))


(defn vec-cross-3d [u v] 
  (vector (det-2x2 (u 1) (u 2) (v 1) (v 2))
          (det-2x2 (u 2) (u 0) (v 2) (v 0))
          (det-2x2 (u 0) (u 1) (v 0) (v 1))))

(defn quat-mul [q1 q2] 
  (let [r1 (first q1) v1 (vec (rest q1))
        r2 (first q2) v2 (vec (rest q2))]
    (cons (- (* r1 r2) (vec-dot v1 v2)) 
          (vec-add (vec-scale r1 v2)
                   (vec-scale r2 v1)
                   (vec-cross-3d v1 v2)))))
