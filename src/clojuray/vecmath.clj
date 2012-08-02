(ns clojuray.vecmath)

; Dot product of two vectors
(defn dot
  [v1 v2]
  (reduce + (map * v1 v2)))

; Normalize a 3D vector
(defn normalize
  [v]
  (let [magnitude (Math/sqrt (dot v v))]
    (into [] (map #(/ % magnitude) v))))

; Cross product of two vectors
(defn cross
  [v1 v2]
  [ (- (* (v1 1) (v2 2)) (* (v1 2) (v2 1)))
    (- (* (v1 2) (v2 0)) (* (v1 0) (v2 2)))
    (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0))) ])

; Scale vector v by scalar s
(defn scale
  [s v]
  (into [] (map #(* % s) v)))

; Magnitude of vector v
(defn magnitude
  [v]
  (Math/sqrt (dot v v)))
