(ns clojuray.image-plane)

; import vecmath functions
(require '(clojuray [vecmath :as vecmath]))

; TEMPORARY DEBUG MACRO: debug let
(defmacro dlet [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) " : " ~v)]))
                   (partition 2 bindings))]
     ~@body))

;; Construct the corners of an image plane from the scene data
(defn compute-plane
  [scene]
  (let [{camera :camera} scene
        {eye :eye, up :up, look-at :look_at, fovy :fovy, aspect :aspect} camera
        fovy (Math/toRadians fovy)
        ; Form orthonormal basis
        w (vecmath/normalize (map - look-at eye))
        u (vecmath/normalize (vecmath/cross up w))
        v (vecmath/cross w u)
        ; Define image plane
        C (into [] (map + eye w))
        y (Math/tan (/ fovy 2.0))
        x (Math/tan (/ (* aspect fovy) 2.0))
        ; Define corners
        LL (map - (map + C (vecmath/scale x u)) (vecmath/scale y v))  ; lower left
        UL (map + (map + C (vecmath/scale x u)) (vecmath/scale y v))  ; upper left
        LR (map - (map - C (vecmath/scale x u)) (vecmath/scale y v))  ; lower right
        UR (map + (map - C (vecmath/scale x u)) (vecmath/scale y v))] ; upper right
    ; computes corners
    {:LL (vec LL), :UL (vec UL), :LR (vec LR), :UR (vec UR)}))

;; Using the image plane, computes the center of the (x, y) pixel in the scene's
;; coordinate system.
(defn get-sample-point
  [image-plane x y width height]
  ; Get normalized value of middle of pixel
  (let [u (/ (+ x 0.5) width)
        v (/ (+ y 0.5) height)
        LL (get image-plane :LL)
        UL (get image-plane :UL)
        LR (get image-plane :LR)
        UR (get image-plane :UR)
        ; Bilinear interpolation between corners
        ; (1-u) * ((1-v)*LL + v*UL)
        ;  +
        ;    u  * ((1-v)*LR + v*UR)
        left  (map + (vecmath/scale (- 1 v) LL) (vecmath/scale v UL))
        right (map + (vecmath/scale (- 1 v) LR) (vecmath/scale v UR))
        ]
    (vec (map + (vecmath/scale (- 1 u) left) (vecmath/scale u right)))))

;; Display the output image of the rendering
(defn display-output-image
  [output-image width height]
  '()
)
