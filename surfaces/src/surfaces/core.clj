(ns surfaces.core
  (:require [quil.core :refer :all]
            [quil.applet :refer [current-applet]]
            [incanter.interpolation :refer :all]))

(def mesh (atom nil))
(def render (atom render))
(def type (atom :surface))
(def n (atom 7))
(def m (atom 8))
(def cur-x (atom 0))
(def cur-y (atom 0))

(def parts 50)

(defn create-surface-mesh [f [u-l u-u] [v-l v-u]]
  (let [surface (proxy [wblut.geom.WB_Surface] []
                  (loweru [] u-l)
                  (lowerv [] v-l)
                  (upperu [] u-u)
                  (upperv [] v-u)
                  (surfacePoint [u v]
                    (let [[x y z] (f u v)]
                      (wblut.geom.WB_Point3d. (double x) (double y) (double z)))))]
    (wblut.hemesh.HE_Mesh. (wblut.hemesh.HEC_FromSurface. surface parts parts false false))))

(defn rand-grid [n m]
  (let [grid (repeatedly n #(repeatedly m rand))
        grid (mapv vec grid)
        grid (reduce (fn [gr i]
                       (assoc-in gr [i (dec m)] (get-in gr [i 0])))
                     grid
                     (range n))
        grid (assoc-in grid [(dec n)] (first grid))]
    grid))

(def ggrid (rand-grid @n @m))

(def interp
  #_(approximate-grid ggrid :degree 1)
  (interpolate-grid ggrid :bicubic-spline))

(defn surface-f [u v]
  [u v (interp (+ u 0.5) (+ v 0.5))])

(defn as-spherical [[x y z]]
  (let [r (+ 0.2 (* z 0.8))
        u (* TWO-PI (+ x 0.5))
        v (* TWO-PI y)]
    [(* (cos v) (cos u) r)
     (* (cos v) (sin u) r)
     (* (sin v) r)]))

(defn setup []
  (let [surface-fn (if (= :surface @type)
                     surface-f
                     (comp as-spherical surface-f))]
    (reset! mesh (create-surface-mesh surface-fn [-0.5 0.5] [-0.5 0.5])))
  (reset! render (wblut.processing.WB_Render. (current-applet))))

(defn uniform-split [[from to] n]
  (->> (range n)
       (map #(/ % (dec n)))
       (map #(* % (- to from)))
       (map #(+ from %))))

(defn draw-points []
  (let [n (count ggrid)
        m (count (first ggrid))
        xs (vec (uniform-split [-0.5 0.5] m))
        ys (vec (uniform-split [-0.5 0.5] n))]
    (stroke-weight 7)
    (doseq [i (range n)
            j (range m)]
      (if (and (= i @cur-y) (= j @cur-x))
        (stroke 255 0 0)
        (stroke 0 0 255))
      (let [p [(xs j) (ys i) (get-in ggrid [i j])]]
        (if (= :surface @type)
          (apply point p)
          (apply point (as-spherical p)))))
    (stroke-weight 1)))

(defn move-current-selection [dx dy]
  (swap! cur-x #(constrain (+ dx %) 0 (dec @m)))
  (swap! cur-y #(constrain (+ dy %) 0 (dec @n))))

(def space (keyword " "))

(defn key-pressed []
  (let [key (key-as-keyword)]
    (cond (= key :left) (move-current-selection 1 0)
          (= key :right) (move-current-selection -1 0)
          (= key :up) (move-current-selection 0 1)
          (= key :down) (move-current-selection 0 -1))))

(defn draw []
  (lights)
  (background 120)
  (push-matrix)
  (translate (/ (width) 2) (/ (height) 2))
  (scale 300)
  (rotate-y (* (mouse-x) (/ 1.0 (width)) TWO-PI))
  (rotate-x (* (mouse-y) (/ 1.0 (height)) TWO-PI))
  (fill 255)
  (no-stroke)
  (.drawFaces @render @mesh)
  (stroke 0)
  (.drawEdges @render @mesh)
  (draw-points)
  #_(do (stroke-weight 10)
      (stroke 255 0 0)
      (line 0 0 0 1 0 0)
      (stroke 0 255 0)
      (line 0 0 0 0 1 0)
      (stroke 0 0 255)
      (line 0 0 0 0 0 1)
      (stroke-weight 1))
  (pop-matrix))


(defsketch example
  :title "Surfaces"
  :setup setup
  :renderer :p3d
  :draw draw
  :key-pressed key-pressed
  :size [800 800])
