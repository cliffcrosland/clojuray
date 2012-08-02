(ns clojuray.sphere)

(require '[clojuray.vecmath :as vecm])
(require '[clojuray.debug :as debug])

;; Create sphere intersection hash-map given a ray, a distance t along the
;; ray, and a sphere object
(defn create-intersection
  [ray t sphere-object]
  (let [E (ray :start) ;ray start
        D (ray :direction) ;ray direction
        {{C :center} :shape} sphere-object ;sphere center
        location (map + E (vecm/scale t D))] ;intersection loc
    {:location location
     :object sphere-object
     :ray-t t
     :normal (vecm/normalize (map - location C))}))


;; Compute the intersection of the ray with this sphere object
;; If there is no intersection, return nil.
(defn intersect
  [ray sphere-object]
  (let [E (ray :start) ; ray start
        D (ray :direction) ; ray direction
        {{C :center} :shape} sphere-object ; sphere center
        {{radius :radius} :shape} sphere-object ;sphere radius
        EminC (map - E C) ; vector from sphere center to ray start
        ; discriminant
        term-1-root (vecm/dot D EminC)
        term-1 (* term-1-root term-1-root)
        term-2 (* (vecm/dot D D)
                  (- (vecm/dot EminC EminC) (* radius radius)))
        discriminant (- term-1 term-2)]
    (cond
      ; if discriminant is less than 0, no solutions, so no intersection
      (< discriminant 0)
      nil
      ; one solution, create intersection
      (= discriminant 0)
      (let [t (/ (* -1 (vecm/dot D EminC)) (vecm/dot D D))]
        (create-intersection ray t sphere-object))
      :else ; two solutions, pick closer intersection
      (let [
            t1 (/ (+ (* -1 (vecm/dot D EminC))
                     (Math/sqrt discriminant))
                 (vecm/dot D D))
            t2 (/ (- (* -1 (vecm/dot D EminC))
                     (Math/sqrt discriminant))
                 (vecm/dot D D))]
        (create-intersection ray (min t1 t2) sphere-object)))))
