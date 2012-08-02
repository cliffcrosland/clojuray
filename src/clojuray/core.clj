(ns clojuray.core)

; import json parsing
(use '[clojure.data.json :only (read-json)])

;import clojuray objects
(require '(clojuray [image-plane :as image-plane]))
(require '(clojuray [ray :as ray]))
(require '(clojuray [vecmath :as vecmath]))
(require '(clojuray [light :as light]))
(require '(clojuray [scene-object :as scene-object]))
(require '[clojuray.debug :as debug])

(require '[clojure.string :as string])

; import java classes
(:import '(java.awt Color BufferedImage Dimension)
         '(javax.swing.JFrame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display output image
(defn display-output-image
  [width height rendered-image]
  (let [image (java.awt.image.BufferedImage.
               width height java.awt.image.BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)]
    (println "Rendering image...")
    (doseq [pixel rendered-image]
      (let [{[r g b] :color} pixel
            {x :x} pixel
            {y :y} pixel]
        (.setColor graphics (java.awt.Color.
                              (min (float r) 1.0)
                              (min (float g) 1.0)
                              (min (float b) 1.0)))
        (.drawLine graphics x y x y)))
    (println "Drawing image on screen...")
    (doto (javax.swing.JFrame.)
      (.add (proxy [javax.swing.JPanel] []
              (paint [g] (.drawImage g image 0 0 this))))
      (.setSize (java.awt.Dimension. width height))
      (.show))))

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
            base-color (light/shading intersection ray lights objects)]
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
                reflect-color (trace-ray reflect-ray scene (dec bounce-depth))]
            ; Sum the base color with the reflected color
            (map + base-color (map * reflect-color mirror-reflectance)))
          ; Otherwise, don't bounce. Just use base color of material.
          base-color)))))

;; Compute the color of the pixel specified by pixel-loc.
(defn compute-pixel-color
  [plane eye width height scene bounce-depth pixel-loc]
  (let [{x :x y :y} pixel-loc
        sample-point (image-plane/get-sample-point plane x y width height)
        ray (ray/ray-through-points eye sample-point)]
    {:color (trace-ray ray scene bounce-depth) :x x :y y}))

;; Render the scene, computing a color for each ray passing through a pixel.
(defn render
  [scene]
  (let [plane (image-plane/compute-plane scene) ; image plane coordinates
        width (reduce get scene [:output :width]) ; pixel width of output
        height (reduce get scene [:output :height]) ; pixel height of output
        eye (reduce get scene [:camera :eye]) ; location of eye in scene
        bounce-depth (scene :bounce_depth) ; number of ray bounce recursions
        pixel-list (for [x (range 0 width) y (range 0 height)]
                    {:x x :y y})] ; pixels to be computed
    ; define shadow bias. makes it so bouncing rays do not re-intersect with
    ; object surface.
    (scene-object/def-shadow-bias (scene :shadow_bias))
    ; Display output image, tracing rays for all x * y pixels.
    (let [image (pmap (partial compute-pixel-color
                        plane eye width height scene bounce-depth)
                pixel-list)]
      (display-output-image width height image))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove comment lines from JSON file.
;; Note: comments are only removed if // are the first two non-whitespace
;; characters on a line.
(defn remove-line-cmnts
  [file-str]
  (string/join
    \newline
    (for [line (string/split-lines file-str)]
      (if (nil? (re-find #"^//" (string/trim line)))
        line ; If the first two chars are //, remove line
        ""))))

;; Parse the json scene and render it
(defn -main
  [& args]
  ; Render the scene
  (time
    (render
      (read-json (remove-line-cmnts (slurp (first args)))))))
