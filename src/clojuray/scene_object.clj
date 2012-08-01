(ns clojuray.scene-object)

(require '[clojuray.vecmath :as V])
(require '[clojuray.sphere :as sphere])
(require '[clojuray.triangle :as triangle])


;; Compute the intersection of the ray with this object.
;; If there is no intersection, eval to nil.
(defn intersect
  [ray object]
  (let [{{shape-type :type} :shape} object]
    (cond
      (= shape-type "sphere") (sphere/intersect ray object)
      (= shape-type "triangle") (triangle/intersect ray object)
      :else nil)))
