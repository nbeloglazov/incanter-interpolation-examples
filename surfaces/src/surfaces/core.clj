(ns surfaces.core
  (:require [quil.core :refer :all]
            [quil.applet :refer [current-applet]]))

(def mesh (atom nil))
(def render (atom render))

(defn create-surface-mesh [f [u-l u-u] [v-l v-u]]
  (let [surface (proxy [wblut.geom.WB_Surface] []
                  (loweru [] u-l)
                  (lowerv [] v-l)
                  (upperu [] u-u)
                  (upperv [] v-u)
                  (surfacePoint [u v]
                    (let [[x y z] (f u v)]
                      (wblut.geom.WB_Point3d. (double x) (double y) (double z)))))]
    (wblut.hemesh.HE_Mesh. (wblut.hemesh.HEC_FromSurface. surface 100 100 false false))))


(defn f [u v]
  (let [r (+ 0.5 (/ u TWO-PI))]
    [(* (cos v) (cos u) r)
     (* (cos v) (sin u) r)
     (* (sin v) r)]))

(defn setup []
  (reset! mesh (create-surface-mesh f [0 TWO-PI] [(- PI) PI]))
  (reset! render (wblut.processing.WB_Render. (current-applet))))

(defn draw []
  (lights)
  (background 120)
  (push-matrix)
  (translate 400 400)
  (rotate-y (* (mouse-x) (/ 1.0 (width)) TWO-PI))
  (rotate-x (* (mouse-y) (/ 1.0 (height)) TWO-PI))
  (scale 100)
  (fill 255)
  (no-stroke)
  (.drawFaces @render @mesh)
  (stroke 0)
  (.drawEdges @render @mesh)
  #_(do (stroke-weight 10)
      (stroke 255 0 0)
      (line 0 0 0 1 0 0)
      (stroke 0 255 0)
      (line 0 0 0 0 1 0)
      (stroke 0 0 255)
      (line 0 0 0 0 0 1)
      (stroke-weight 1))
  
  (pop-matrix))


(defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :renderer :p3d
  :draw draw                        ;;Specify the draw fn
  :size [800 800])                  ;;You struggle to beat the golden ratio
