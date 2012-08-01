(ns clojuray.ray)

(require '[clojuray.vecmath :as vecmath])
(require '[clojuray.scene-object :as scene-object])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray
;; Consists of a start point and a normalized direction vector
;; TODO: Make a strict type (defrecord?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Computes a ray that goes from the start through the sample point
(defn ray-through-points
  [start sample-opint]
  {:start start
   :direction (vecmath/normalize (map - sample-point start))})

;; Computes a ray that goes from the start in the specified direction
(def ray-at-point-in-direction
  [start direction]
  {:start start
   :direction (vecmath/normalize direction)})

;; Compute the closest intersection point along the ray with a scene object
(def intersect
  [ray objects]
  (let [intersections ; get all of the non-nil intersections
        (filter #(not (nil? %)) (map (partial scene-object/intersect ray) objects))]
    (if (= 0 (count intersections))
      ; if there are no intersections, eval to nil
      nil
      ; otherwise, eval to the closest intersection (least distance t
      ; along ray) 
    (apply min-key :ray-t intersections))))
