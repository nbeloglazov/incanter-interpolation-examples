(ns function-interpolation.core
  (:require [quil.core :refer :all]))

(defn col [val]
  (map #(bit-and 0xFF %)
       [(bit-shift-right val 16)
        (bit-shift-right val 8)
        val]))


(def points (atom []))

(def point-size 15)

(def point-color (col 0xFFC0504D))

(defn spy [v] (println v) v)

(def base-mouse-point (atom nil))

(def dragged-point (atom nil))

(def checkboxes (atom (into {} [[:parametric false ]
                                [:linear false]
                                [:polynomial true]
                                [:cubic-spline false]
                                [:b-spline false]])))

(def checkbox-text {:parametric "Parametric function"
                    :linear "Linear"
                    :polynomial "Polynomial"
                    :cubic-spline "Cubic spline"
                    :b-spline "B-spline 3 order"})

(def colors {:parametric (col 0)
             :linear (col 0x9BBB59)
             :polynomial (col 0x8064A2)
             :cubic-spline (col 0x4BACC6)
             :b-spline (col 0xF79646)})

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

(defn find-checkbox [[x y]]
  (let [cb-x (+ 30 (- (width) 200))
        belongs? (fn [[ind [id _]]]
                   (and (<= cb-x x (+ cb-x 20))
                        (<= (* 50 (inc ind)) y (+ 20 (* 50 (inc ind))))))]
    (->> @checkboxes
         indexed
         (filter belongs?)
         first
         last
         first)))

(defn mouse-point []
  [(mouse-x) (mouse-y)])

(defn mouse-clicked []
  (println "Mouse clicked")
  (if-let [id (find-checkbox (mouse-point))]
    (swap! checkboxes update-in [id] not)
    (swap! points
         (if (= (mouse-button) :left) conj remove-point)
         (mouse-point))))

(defn mouse-pressed []
  (println "Mouse pressed")
  (let [mouse (mouse-point)]
    (reset! base-mouse-point mouse)
    (reset! dragged-point (find-last-point @points mouse))))

(defn mouse-dragged []
  (when-let [ind @dragged-point]
    (swap! points assoc-in [ind] (mouse-point))))

(defn draw-checkbox [[id checked] x y]
  (with-translation [x y]
    (fill 255)
    (apply stroke (colors id))
    (rect 0 0 20 20)
    (apply fill (colors id))
    (text (checkbox-text id) 25 15)
    (when checked
      (line 0 0 20 20)
      (line 0 20 20 0))))

(defn draw-points [points]
  (stroke-weight 1)
  (stroke 0)
  (doseq [[ind [x y]] (indexed points)]
    (apply fill point-color)
    (ellipse x y point-size point-size)
    (fill 0)
    (text (str (inc ind)) (- x 4) (+ y 20))))

(defn draw-control-panel []
  (stroke-weight 2)
  (with-translation [(- (width) 200) 0]
    (line 0 0 0 (height))
    (doseq [[ind checkbox] (indexed @checkboxes)]
      (draw-checkbox checkbox 30 (* 50 (inc ind))))))

(defn draw []
  (background 255)
  (draw-points @points)
  (draw-control-panel))

(defn setup []
  (frame-rate 5))

(defn run []
  (sketch
   :title "Function interpolation"
   :setup setup
   :draw draw
   :size [800 600]
   :mouse-clicked mouse-clicked
   :mouse-pressed mouse-pressed
   :mouse-dragged mouse-dragged))

#_(run)