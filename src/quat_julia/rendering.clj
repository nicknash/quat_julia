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

(defn get-normal 
  "Compute a normal to the z-buffer at buf-x, buf-y by differencing neighbours"
                 [buf-x buf-y 
                  x-buffer-to-viewport y-buffer-to-viewport 
                  zbuffer size]
  (let [b-left  (clamp-range (- buf-x 1) size)
        b-right (clamp-range (+ buf-x 1) size)
        vp-left (x-buffer-to-viewport b-left)
        vp-right (x-buffer-to-viewport b-right)
        zbuf-left (aget zbuffer b-left buf-y)
        zbuf-right (aget zbuffer b-right buf-y)
        u [(- vp-left vp-right) 0 (- zbuf-left zbuf-right)]
        
        b-above (clamp-range (- buf-y 1) size)
        b-below (clamp-range (+ buf-y 1) size)
        vp-above (y-buffer-to-viewport b-above)
        vp-below (y-buffer-to-viewport b-below)
        zbuf-below (aget zbuffer buf-x b-below)
        zbuf-above (aget zbuffer buf-x b-above)
        v [0 (- vp-below vp-above) (- zbuf-below zbuf-above)]]
    (vec-normalize (vec-cross-3d u v))))
   
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
                                                       [0.3 0.5 0.4] [0.1 0.7 0.8] 15)                
                                          (phong-light normal (vec-normalize [0 0 -1]) 
                                                       (vec-normalize [0.1 0.1 -1]) 
                                                       [0.05 0.05 0.2] [0.1 0.1 0.3] 50)))]
        (vec (map #(clamp-range % 255) (vec-scale 256 light-vec))))
      [0 0 0])))
  
