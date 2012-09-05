(ns quat-julia.display
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage))
 (:use quat-julia.rendering)
 (:use quat-julia.vec-utils)
 (:use quat-julia.naive-raymarch)
 (:use quat-julia.unbounding-raymarch))

(defn paint-canvas [zbuf
                    {x-buffer-to-viewport :x-buffer-to-viewport 
                     y-buffer-to-viewport :y-buffer-to-viewport} graphics] 
  (let [size (:size zbuf)]
    (doseq [y (range size)
            x (range size)]
      (let [col-vec (get-color x y x-buffer-to-viewport y-buffer-to-viewport zbuf)]
        (.setColor graphics (Color. (col-vec 0) (col-vec 1) (col-vec 2))))
      (.drawLine graphics x y x y))))

(defn draw [zbuf
            viewport-funcs]
  (let [size (:size zbuf)
        image  (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]
    (paint-canvas zbuf viewport-funcs (.createGraphics image))
    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. size size))
      (.show))))
 

(def julia-zbuffer {:zbuffer (make-array Float/TYPE 50 50) :size 50 :min-z -1 :max-z 1}) 
  
(def viewport-funcs 
  {:x-buffer-to-viewport (fn [x] (buffer-to-viewport x (:size julia-zbuffer) -1.3 1.3))
   :y-buffer-to-viewport (fn [y] (buffer-to-viewport y (:size julia-zbuffer) -1.1 1.1))})

(defn get-julia-z [vp-x vp-y]
  (unbounding-ray-march [-0.2 0.4 0.4 -0.4] 12 0 15 0.0000001 -1 vp-x vp-y))

; Alternatively, here's an example of using the naive ray marcher:
;(defn get-julia-z [vp-x vp-y]
;  (ray-march-julia [-0.2 0.4 0.4 -0.4] 12 0 75 600 
;                   (:min-z julia-zbuffer) (:max-z julia-zbuffer) vp-x vp-y))
 
(time (fill-zbuffer get-julia-z viewport-funcs julia-zbuffer))
(time (draw julia-zbuffer viewport-funcs))
