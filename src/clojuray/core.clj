(ns clojuray.core)

; import json parsing
(use '[clojure.data.json :only (read-json)])

;import clojuray objects
(require '(clojuray [image-plane :as image-plane]))
(require '(clojuray [ray :as ray]))
(require '(clojuray [vecmath :as vecmath]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display output image
(defn display-output-image
  [width height output-image]
  (println "count : output-image")
  (println (count output-image))
)

;; Trace the ray, computing its color
(defn trace-ray
  [ray scene bounce-depth]
  (let [objects (scene :objects)
        intersection (ray/intersect ray objects)]
    (if (nil? intersection)
      ; if ray does not touch an object, render black
      [0 0 0]
      ; otherwise, compute the color bounced from that object
      (let [object (intersection :object)
            material (object :material)
            lights (scene :lights)
            mirror-reflectance (material :mirror)
            base-color (ray/shading intersection ray lights objects)]
        ; If we should reflect, bounce ray off object, and add reflected
        ; color to base color
        (if (and (> bounce-depth 0) (not= mirror-reflectance [0.0 0.0 0.0]))
          (let [normal (intersection :normal)
                cur-direction (ray :direction)
                ; reflection direction =
                ; current-dir - 2 * dot(current-dir, normal) * normal
                reflect-direction
                  (map - cur-direction
                         (vecmath/scale (* 2 (vecmath/dot cur-direction normal))
                                        normal))
                int-loc (intersection :location)
                reflect-ray (ray/ray-at-point-in-direction
                              int-loc reflect-direction)
                ; Recursive step
                reflect-color (trace-ray reflect-ray scene (decr bounce-depth))]
            ; Sum the base color with the reflected color
            (map + base-color (map * reflect-color mirror-reflectance)))
          ; Otherwise, don't bounce. Just use base color of material.
          base-color)))))

;; Render the scene, computing a color for each ray passing through a pixel.
(defn render
  [scene]
  (let [plane (image-plane/compute-plane scene) ; image plane coordinates
        width (reduce get scene [:output :width]) ; pixel width of output
        height (reduce get scene [:output :height]) ; pixel height of output
        eye (reduce get scene [:camera :eye]) ; location of eye in scene
        bounce-depth (scene :bounce_depth)] ; number of ray bounce recursions
    (display-output-image width height
      (for [x (range 0 width)
            y (range 0 height)]
        (let [sample-point (image-plane/get-sample-point plane x y width height)
              ray (ray/ray-through-points eye sample-point)]
          (trace-ray ray scene bounce-depth)))))) ; compute the color of ray

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse the json scene and render it
(defn -main
  [& args]
  ; Render the scene
  (render
    (read-json
      (slurp (first args))))
