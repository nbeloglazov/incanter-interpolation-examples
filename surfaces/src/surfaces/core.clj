(ns surfaces.core
  (:require [quil.core :refer :all]
            [quil.applet :refer [current-applet]]
            [incanter.interpolation :refer :all]
            [clojure.string :as string])
  (:gen-class))

(defn col [val]
  (map #(bit-and 0xFF %)
       [(bit-shift-right val 16)
        (bit-shift-right val 8)
        val]))

(def parts 70)
(def max-z 0.5)
(def render (atom render))
(def s-type (atom :surface))
(def n (atom 7))
(def m (atom 7))
(def cur-x (atom 0))
(def cur-y (atom 0))
(def grid (atom nil))
(def active (atom #{:bilinear}))
(def queue (java.util.concurrent.LinkedBlockingDeque.))
(def in-queue (atom #{}))
(def in-progress (atom #{}))
(def draw-stats? (atom true))


(def meshes {:bilinear (atom nil)
             :polynomial (atom nil)
             :bicubic-spline-natural (atom nil)
             :bicubic-spline-closed (atom nil)
             :bicubic-hermite (atom nil)
             :b-surface (atom nil)
             :linear-least-squares (atom nil)})

(def colors {:bilinear (col 0x9BBB59)
             :polynomial (col 0x8064A2)
             :bicubic-spline-natural (col 0x4BACC6)
             :bicubic-spline-closed (col 0xC0504D)
             :bicubic-hermite (col 0x1F497D)
             :b-surface (col 0xF79646)
             :linear-least-squares (col 0x00AA00)})

(def all-types (keys meshes))

(defn put-job [queue job]
  (when-not (@in-queue job)
    (swap! in-queue conj job)
    (.put queue job)))

(defn get-job [queue]
  (let [job (.take queue)]
    (swap! in-queue disj job)
    job))

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
  (let [rand-z #(* max-z (rand))
        grid (repeatedly n #(repeatedly m rand-z))
        grid (mapv vec grid)
        grid (reduce (fn [gr i]
                       (assoc-in gr [i (dec m)] (get-in gr [i 0])))
                     grid
                     (range n))
        grid (assoc-in grid [(dec n)] (first grid))]
    grid))

(defn regenerate-grid [type]
  (reset! grid (case type
                 :flat (vec (repeat @n (vec (repeat @m 0))))
                 :random (rand-grid @n @m))))

(defn as-spherical [[x y z]]
  (let [r (+ 0.2 (* z 0.8))
        u (* TWO-PI (+ x 0.5))
        v (* TWO-PI y)]
    [(* (cos v) (cos u) r)
     (* (cos v) (sin u) r)
     (* (sin v) r)]))

(defn interpolate-grid-by-type [type]
  (case type
    :bicubic-spline-natural (interpolate-grid @grid :bicubic :boundaries :natural)
    :bicubic-spline-closed (interpolate-grid @grid :bicubic :boundaries :closed)
    :linear-least-squares (interpolate-grid @grid :linear-least-squares
                                            :basis (fn [x y] [1 x y (Math/sin x) (Math/sin y) (Math/cos y) (Math/cos x)]))
    (interpolate-grid @grid type)))

(defn recalculate-mesh [type]
  (let [interp (interpolate-grid-by-type type)
        surface-f (fn [u v] [u v (interp (+ u 0.5) (+ v 0.5))])
        surface-f (if (= :surface @s-type) surface-f (comp as-spherical surface-f))]
    (reset! (meshes type) (create-surface-mesh surface-f [-0.5 0.5] [-0.5 0.5]))))

(defn process-job [queue]
  (let [type (get-job queue)]
    (swap! in-progress conj type)
    (recalculate-mesh type)
    (swap! in-progress disj type)
    (recur queue)))

(defn start-workers []
  (dotimes [_ (max 1 (dec (.. Runtime getRuntime availableProcessors)))]
    (future (process-job queue))))

(defn recalculate-meshes-async [all?]
  (let [ active @active
        non-active (remove active all-types)]
   (doseq [type (concat active (if all? non-active []))]
     (put-job queue type))))

(defn setup []
  (regenerate-grid :random)
  (reset! render (wblut.processing.WB_Render. (current-applet)))
  (start-workers)
  (recalculate-meshes-async true))

(defn uniform-split [[from to] n]
  (->> (range n)
       (map #(/ % (dec n)))
       (map #(* % (- to from)))
       (map #(+ from %))))

(defn draw-points []
  (let [grid @grid
        n (count grid)
        m (count (first grid))
        xs (vec (uniform-split [-0.5 0.5] m))
        ys (vec (uniform-split [-0.5 0.5] n))]
    (stroke-weight 7)
    (doseq [i (range n)
            j (range m)]
      (if (and (= i @cur-y) (= j @cur-x))
        (stroke 255 0 0)
        (stroke 0 0 255))
      (let [p [(xs j) (ys i) (get-in grid [i j])]]
        (if (= :surface @s-type)
          (apply point p)
          (apply point (as-spherical p)))))
    (stroke-weight 1)))

(defn draw-meshes []
  (doseq [type @active]
    (when-let [mesh @(meshes type)]
      (apply fill (colors type))
      (no-stroke)
      (.drawFaces @render mesh)
      (stroke 0)
      (.drawEdges @render mesh))))

(defn build-string [name types]
  (str name ": " (string/join "," (map clojure.core/name types))))

(defn indexed [coll]
  (map vector coll (range)))

(defn draw-stats []
  (push-matrix)
  (translate 200 10)
  (let [in-progress (into @in-progress @in-queue)
        done (remove in-progress all-types)]
    (fill 0)
    (text-size 20)
    (text (build-string "Done" done) 20 20 0)
    (text (build-string "In progress" in-progress) 20 40 0))
  (translate -200 20)
  (text "Types:" 20 0 0)
  (doseq [[type i] (indexed all-types)]
    (apply fill (colors type))
    (text (str (inc i) ". " (name type)) 20 (+ 20 (* i 20)) 0))
  (let [shortcuts ["Keyboard shortcuts:"
                   "1-7 - toggle different interpolators"
                   "a/z - raise/lower currently selected point"
                   "left/right/up/down - move selection"
                   "r/o - regenerate all/visible surfaces"
                   "d/f - regenerate grid random/flat"
                   "space - toggle between surface/sphere"
                   "s - show/hide text"]]
    (fill 0)
    (translate 0 180)
    (doseq [[shortcut i] (indexed shortcuts)]
      (text shortcut 20 (* i 20))))
  (pop-matrix))

(defn draw []
  (lights)
  (background 200)
  (when @draw-stats? (draw-stats))
  (push-matrix)
  (translate (/ (width) 2) (/ (height) 2))
  (scale 500)
  (rotate-y (* (mouse-x) (/ 1.0 (width)) TWO-PI))
  (rotate-x (* (mouse-y) (/ 1.0 (height)) TWO-PI))
  (draw-meshes)
  (draw-points)
  (pop-matrix))

(defn move-current-selection [dx dy]
  (swap! cur-x #(constrain (+ dx %) 0 (dec @m)))
  (swap! cur-y #(constrain (+ dy %) 0 (dec @n))))

(defn change-grid [i j delta]
  (swap! grid update-in [i j] #(constrain (+ % delta) 0 max-z))
  (when (or (= 0 i) (= (dec @n) i))
    (swap! grid assoc-in [(- @n i 1) j] (get-in @grid [i j])))
  (when (or (= 0 j) (= (dec @m) j))
    (swap! grid assoc-in [i (- @m j 1)] (get-in @grid [i j])))
  (when (and (or (= 0 i) (= (dec @n) i))
             (or (= 0 j) (= (dec @m) j)))
    (swap! grid assoc-in [(- @n i 1) (- @m j 1)] (get-in @grid [i j])))
  (recalculate-meshes-async false))

(defn toggle [set value]
  ((if (set value) disj conj) set value))

(defn toggle-active [value]
  (swap! active toggle value))

(defn toggle-surface-type []
  (swap! s-type {:surface :sphere :sphere :surface}))

(def space (keyword " "))

(defn key-pressed []
  (let [key (key-as-keyword)]
    (if (= space key)
      (toggle-surface-type)
      (case key
        :left  (move-current-selection  1  0)
        :right (move-current-selection -1  0)
        :up    (move-current-selection  0  1)
        :down  (move-current-selection  0 -1)
        :a (change-grid @cur-y @cur-x  0.1)
        :z (change-grid @cur-y @cur-x -0.1)
        :1 (toggle-active :bilinear)
        :2 (toggle-active :polynomial)
        :3 (toggle-active :bicubic-spline-natural)
        :4 (toggle-active :bicubic-spline-closed)
        :5 (toggle-active :bicubic-hermite)
        :6 (toggle-active :b-surface)
        :7 (toggle-active :linear-least-squares)
        :r (recalculate-meshes-async true)
        :o (recalculate-meshes-async false)
        :d (regenerate-grid :random)
        :f (regenerate-grid :flat)
        :s (swap! draw-stats? not)
        :default))))

(defn exit-on-close [sketch]
  (let [frame (-> sketch .getParent .getParent .getParent .getParent)]
    (.setDefaultCloseOperation frame javax.swing.JFrame/EXIT_ON_CLOSE)))


(defn run []
  (sketch
   :title "Surfaces"
   :setup setup
   :renderer :p3d
   :draw #'draw
   :key-pressed key-pressed
   :size [1200 1200]))

(defn -main [& args]
  (exit-on-close (run)))
