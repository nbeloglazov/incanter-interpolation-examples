(ns surfaces.core
  (:require [quil.core :refer :all]
            [quil.applet :refer [current-applet]]))

(def mesh (atom nil))
(def render (atom render))

(defn create-mesh []
  (let [creator (wblut.hemesh.HEC_Cube. 300 1 1 1)
        m (wblut.hemesh.HE_Mesh. creator)]
    (.modify m
             (doto (wblut.hemesh.HEM_Extrude.)
               (.setDistance 300.0)))
    (reset! mesh
            (wblut.hemesh.HE_Mesh.
             (doto (wblut.hemesh.HEC_FromFrame.)
               (.setFrame m)
               (.setMaximumStrutLength 20))))))

(defn setup []
  (create-mesh)
  (let [modifier (wblut.hemesh.HEM_SphereInversion.)]
    (doto modifier
      (.setRadius 200)
      (.setCenter 50 0 0)
      (.setCutoff 1000)
      (.setLinear false))
    (.modify @mesh modifier))
  (reset! render (wblut.processing.WB_Render. (current-applet))))

(defn draw []
  (background 120)
  (directional-light 255 255 255 1 1 -1)
  (directional-light 127 127 127 -1 -1 1)
  (translate 400 400 0)
  (rotate-y (* (mouse-x) (/ 1.0 (width)) TWO-PI))
  (rotate-x (* (mouse-y) (/ 1.0 (height)) TWO-PI))
  (fill 255)
  (no-stroke)
  (.drawFaces @render @mesh)
  (stroke 0)
  (.drawEdges @render @mesh))


(defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :renderer :p3d
  :draw draw                        ;;Specify the draw fn
  :size [800 800])                  ;;You struggle to beat the golden ratio
