(ns clojuray.light)

(require '[clojuray.vecmath :as vecmath])
(require '[clojuray.ray :as ray])
(require '[clojuray.scene-object :as scene-object])
(require '[clojuray.debug :as debug])

;; Direction to the light from the intersection
(defn dir-to-light-from-location
  [light location]
  (cond
    ; ambient light has no direction
    (= (light :type) "ambient")
    [0.0 0.0 0.0]
    ; directional light, return opposite of direction
    (= (light :type) "directional")
    (vecmath/normalize (vecmath/scale -1 (light :direction)))
    ; point light
    :else
    (vecmath/normalize (map - (light :position) location))))

;; Evaluates to true if there is an object blocking the light from reaching the
;; intersection point.
(defn occluder-exists
  [intersection light objects]
    (some true? ; if the for expr ever generates true, there is an occluder
      (for [object objects]
        (let [p (intersection :location)
              int-to-light (dir-to-light-from-location light p)
              r (ray/ray-at-point-in-direction p int-to-light)
              pot-occl-intersection (scene-object/intersect r object)]
          (if (nil? pot-occl-intersection)
            false ; if there is no potential occluder, eval to false.
            ; otherwise, check to see if occluder is btwn light
            ; and intersection surface.
            (let [q (pot-occl-intersection :location)
                  occluder-to-light (dir-to-light-from-location light q)]
              ; if dot product is positive, then occluder is between
              ; intersection surface and light.
              (> (vecmath/dot int-to-light occluder-to-light) 0)))))))

;; Compute the color shading for the given intersection and ray, using the
;; given lights and objects (which might occlude the lights).
(defn shading
  [intersection ray lights objects]
  (let [{{material :material } :object} intersection]
    ; Sum over all light contributions
    (apply map +
      (for [light lights]
        (if (= (light :type) "ambient")
          ; if the light is ambient, simply multiply light color by
          ; material's ambient color
          (map * (material :ambient) (light :color))
          ; otherwise, compute contribution of point and directional light
          (if (occluder-exists intersection light objects)
            ; if an occluder blocks light, it does not contribute
            [0.0 0.0 0.0]
            ; otherwise, compute contribution of diffuse and specular light
            (let [L (dir-to-light-from-location light (intersection :location))
                  V (ray :direction)
                  N (intersection :normal)
                  R (map - L (vecmath/scale
                               (* 2 (vecmath/dot L N))
                               N))
                  ; Compute diffuse light contribution
                  diffuse-dot (max (vecmath/dot L N) 0.0)
                  diffuse-term (vecmath/scale
                                 diffuse-dot
                                 (map * (material :diffuse) (light :color)))
                  ; Compute specular light term
                  specular-dot (Math/pow
                                 (max (vecmath/dot R V) 0)
                                 (material :shine))
                  specular-term (vecmath/scale
                                  specular-dot
                                  (map * (material :specular) (light :color)))]
              (map + diffuse-term specular-term))))))))
