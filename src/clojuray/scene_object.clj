(ns clojuray.scene-object)

(require '[clojuray.sphere :as sphere])
(require '[clojuray.triangle :as triangle])

;; Set shadow bias to default 0.0001
(def shadow-bias 0.0001)

;; Define the shadow bias. Used so that bouncing rays do not re-intersect
;; with object surface
(defn def-shadow-bias
  [shadow-bias-arg]
  (def shadow-bias shadow-bias-arg))

;; Compute the intersection of the ray with this object.
;; If there is no intersection, eval to nil.
(defn intersect
  [ray object]
  (let [{{shape-type :type} :shape} object]
    (cond
      (= shape-type "sphere") (sphere/intersect ray object shadow-bias)
      (= shape-type "triangle") (triangle/intersect ray object shadow-bias)
      :else nil)))

