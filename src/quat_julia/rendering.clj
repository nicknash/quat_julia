(ns quat-julia.rendering
  "Z-buffer rendering functions: a minimal directional Blinn-Phong implementation with 
   depth cue and normal generation by differencing."
  (:use quat-julia.vec-utils))

(defn buffer-to-viewport [buf-pos buf-size viewport-min viewport-max]
  (float (+ viewport-min (* (/ buf-pos buf-size) (- viewport-max viewport-min)))))

(defn fill-zbuffer [get-z                            
                    {x-buffer-to-viewport :x-buffer-to-viewport 
                     y-buffer-to-viewport :y-buffer-to-viewport}
                    {zbuffer :zbuffer size :size}]
  (doseq [buf-y (range size)
          buf-x (range size)]     
    (let [x (x-buffer-to-viewport buf-x) y (y-buffer-to-viewport buf-y)]
      (aset zbuffer buf-x buf-y (float (get-z x y))))))
                 
(defn zero-clamp [v] (if (< v 0) 0 v))   
(defn clamp-range [v r] (int (if (< v 0) 0 (if (> v r) r v))))
  
(defn phong-light [normal view-dir light-dir diffuse specular shininess]
  (let [half-vec (vec-scale 0.5 (vec-add view-dir light-dir))
        n-dot-l (vec-dot normal light-dir)
        clamped-diffuse (zero-clamp n-dot-l)
        h-dot-v (vec-dot half-vec view-dir)
        clamped-specular (zero-clamp h-dot-v)
        spec-exp (Math/pow clamped-specular shininess)]
    (vec-add (vec-scale clamped-diffuse diffuse) (vec-scale spec-exp specular))))

(defn get-buf-vector [buf-pos buf-pos-to-viewport buf-getter size]
  (let [before (clamp-range (- buf-pos 1) size)
        after (clamp-range (+ buf-pos 1) size)
        vp-before (buf-pos-to-viewport before)
        vp-after (buf-pos-to-viewport after)
        buf-before (buf-getter before)
        buf-after (buf-getter after)]    
        [(- vp-before vp-after) (- buf-before buf-after)]))


(defn get-normal 
  "Compute a normal to the z-buffer at buf-x, buf-y by differencing neighbours"
                 [buf-x buf-y 
                  x-buffer-to-viewport y-buffer-to-viewport 
                  zbuffer size]
  (let [u (get-buf-vector buf-x x-buffer-to-viewport #(aget zbuffer % buf-y) size)
        v (get-buf-vector buf-y y-buffer-to-viewport #(aget zbuffer buf-x %) size)]
       (vec-normalize (vec-cross-3d (vector (u 0) 0 (v 1)) (vector 0 (v 0) (v 1))))))
  
(defn depth-cue [z min-z max-z] (- 1 (/ (- z min-z) (- max-z min-z))))
    
(defn get-color [buf-x buf-y x-buffer-to-viewport y-buffer-to-viewport 
                 {zbuffer :zbuffer size :size min-z :min-z max-z :max-z}]
  (let [z (aget zbuffer buf-x buf-y)] 
    (if (< z 1)
      (let [normal (get-normal buf-x buf-y x-buffer-to-viewport y-buffer-to-viewport
                               zbuffer (- size 1))
            ; The constants below are chosen from quick experimentation, and define the
            ; two lights used for rendering the z-buffer
            light-vec (vec-scale (depth-cue z min-z max-z)
                                 (vec-add (phong-light normal (vec-normalize [0 0 -1])
                                                       (vec-normalize [-0.2 -0.1 -1])
                                                       [0.5 0.5 0.4] [0.5 0.7 0.3] 15)                
                                          (phong-light normal (vec-normalize [0 0 -1]) 
                                                       (vec-normalize [0.1 0.1 -1]) 
                                                       [0.05 0.2 0.05] [0.1 0.3 0.1] 50)))]
        (vec (map #(clamp-range % 255) (vec-scale 256 light-vec))))
      [0 0 0])))
  
