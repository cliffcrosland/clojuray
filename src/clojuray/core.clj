(ns clojuray.core)

; import json parsing
(use '[clojure.data.json :only (read-json)])

;import clojuray objects
(require '(clojuray [image-plane :as image-plane]))
(require '(clojuray [ray :as ray]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trace the ray, computing its color
(defn trace-ray [ray]
  '[0.5 0.5 0.5]) ; temporary gray color

;; Render the scene (json object)
(defn render [scene]
  (let [image-plane (image-plane/compute-plane scene) ; image plane coordinates
        width (reduce get scene [:output :width]) ; pixel width of output
        height (reduce get scene [:output :height]) ; pixel height of output
        output-image '[]]
    (for [x (range 0 width)
          y (range 0 height)]
      (let [sample-point (image-plane/get-sample-point image-plane x y width height)
            ray (ray/ray-through-point sample-point)
            color (trace-ray ray)]
        (conj output-image color)
    (image-plane/display-output-image output-image width height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse the given json scene and render it
(defn -main
  [& args]
  ; Get the raw json file
  (def raw-scene (slurp (first args)))
  ; Read it into 'scene'
  (def scene (read-json raw-scene))
  ; Render the scene
  (render scene))
