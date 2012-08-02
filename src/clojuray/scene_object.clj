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

;; If the intersection fails the shadow bias (i.e. it's distance along
;; the ray, :ray-t, is less than the shadow bias) eval to nil.
;; Otherwise, eval to the intersection
(defn passes-shadow-bias
  [intersection]
  (if (or (nil? intersection)
          (< (intersection :ray-t) shadow-bias))
    nil
    intersection))

;; Compute the intersection of the ray with this object.
;; If there is no intersection, eval to nil.
(defn intersect
  [ray object]
  (let [{{shape-type :type} :shape} object]
    (cond
      (= shape-type "sphere")
      (passes-shadow-bias (sphere/intersect ray object))
      (= shape-type "triangle")
      (passes-shadow-bias (triangle/intersect ray object))
      :else nil)))

