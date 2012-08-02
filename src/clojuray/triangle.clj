(ns clojuray.triangle)

(require '[clojuray.vecmath :as vecm])
(require '[clojuray.debug :as debug])

;; Compute intersection of this triangle with the ray
(defn intersect
  [ray triangle-object shadow-bias]
  (let [E (ray :start) ;ray start
        D (ray :direction) ;ray direction
        {{v1 :vertex1
          v2 :vertex2
          v3 :vertex3} :shape} triangle-object ;triangle vertices
        ; Compute intersection of ray with triangle.
        ; TODO: link to explanation
        ; triangle side 1
        side1 (vec (map - v3 v1))
        a (side1 0)
        b (side1 1)
        c (side1 2)
        ; triangle side 2
        side2 (vec (map - v3 v2))
        d (side2 0)
        e (side2 1)
        f (side2 2)
        ; ray direction
        g (D 0)
        h (D 1)
        i (D 2)
        ; result vector
        result-vec (vec (map - v3 E))
        j (result-vec 0)
        k (result-vec 1)
        l (result-vec 2)
        ; Denominator term
        M (+
            (* a (- (* e i) (* h f)))
            (* b (- (* g f) (* d i)))
            (* c (- (* d h) (* e g))))
        ; intersection value t (distance along ray to intersection)
        t (* -1 (/
                  (+
                     (* f (- (* a k) (* j b)))
                     (* e (- (* j c) (* a l)))
                     (* d (- (* b l) (* k c))))
                  M))
        ; gamma: if less than 0 or greater than 1, no intersection
        gamma (/
                (+
                  (* i (- (* a k) (* j b)))
                  (* h (- (* j c) (* a l)))
                  (* g (- (* b l) (* k c))))
                M)
        ; beta: if less than 0 or greater than 1 - gamma, no intersection
        beta (/
               (+
                 (* j (- (* e i) (* h f)))
                 (* k (- (* g f) (* d i)))
                 (* l (- (* d h) (* e g))))
               M)]
    (if (or
          (or (< t shadow-bias) (> t Integer/MAX_VALUE))
          (or (< gamma 0) (> gamma 1))
          (or (< beta 0) (> beta (- 1 gamma))))
      ; no intersection
      nil
      ; otherwise, compute intersection with normal pointing toward user
      (let [location (map + E (vecm/scale t D))
            ; choose the normal pointing toward the user
            ; triangle sides originating at vertex 3
            s1 (vec (map - v1 v3))
            s2 (vec (map - v2 v3))
            V (vec (map - E location))
            V-mag (vecm/magnitude V)
            ; normals, in opposite directions
            N1 (vecm/cross s1 s2)
            N2 (vecm/cross s2 s1)
            N1-mag (vecm/magnitude N1)
            N2-mag (vecm/magnitude N2)
            ; choose normal with smaller angle
            angle1 (Math/acos (/ (vecm/dot V N1) (* V-mag N1-mag)))
            angle2 (Math/acos (/ (vecm/dot V N2) (* V-mag N2-mag)))
            normal (vecm/normalize (if (< angle1 angle2) N1 N2))]
        {:location location
         :object triangle-object
         :ray-t t
         :normal normal}))))
