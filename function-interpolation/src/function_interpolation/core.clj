(ns function-interpolation.core
  (:use quil.core))

(def points (atom []))

(def point-size 10)

(def point-color [255 0 0])

(defn setup []
  (frame-rate 5))

(defn spy [v] (println v) v)

(def base-mouse-point (atom nil))

(def dragged-point (atom nil))

(defn indexed [coll]
  (map-indexed vector coll))

(defn remove-nth [coll n]
  (->> (indexed coll)
       (remove #(= (first %) n))
       (mapv second)))

(defn find-last-point [points [x y]]
  (letfn [(belongs? [[p-x p-y]]
            (<= (dist x y p-x p-y) (/ point-size 2)))]
    (->> (indexed points)
         (filter #(belongs? (second %)))
         last
         first)))

(defn remove-point [points point]
  (if-let [ind (find-last-point points point)]
    (remove-nth points ind)
    points))

(defn mouse-point []
  [(mouse-x) (mouse-y)])

(defn mouse-clicked []
  (println "Mouse clicked")
  (swap! points
         (if (= (mouse-button) :left) conj remove-point)
         (mouse-point)))

(defn mouse-pressed []
  (println "Mouse pressed")
  (let [mouse (mouse-point)]
    (reset! base-mouse-point mouse)
    (reset! dragged-point (find-last-point @points mouse))))

(defn mouse-dragged []
  (when-let [ind @dragged-point]
    (swap! points assoc-in [ind] (mouse-point))))

(defn draw-points [points]
  (doseq [[ind [x y]] (indexed points)]
    (apply fill point-color)
    (ellipse x y point-size point-size)
    (fill 0)
    (text (str (inc ind)) (- x 4) (+ y 17))))

(defn draw []
  (background 255)
  (draw-points @points))

(defn run []
  (sketch
   :title "Function interpolation"
   :setup setup
   :draw draw
   :size [800 600]
   :mouse-clicked mouse-clicked
   :mouse-pressed mouse-pressed
   :mouse-dragged mouse-dragged))

(run)