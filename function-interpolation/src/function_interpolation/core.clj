(ns function-interpolation.core
  (:require [quil.core :refer :all]
            [quil.helpers.drawing :refer (line-join-points)]
            [incanter.interpolation :refer (interpolate approximate)]))

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
                                [:linear true]
                                [:polynomial false]
                                [:cubic-spline false]
                                [:b-spline false]])))

(def checkbox-text {:parametric "Parametric function"
                    :linear "Linear"
                    :polynomial "Polynomial"
                    :cubic-spline "Cubic spline"
                    :b-spline "B-spline 3 order"})

(def work-future (atom (future)))

(def curves (atom {}))

(def colors {:parametric (col 0)
             :linear (col 0x9BBB59)
             :polynomial (col 0x8064A2)
             :cubic-spline (col 0x4BACC6)
             :b-spline (col 0xF79646)})

(def interpolators
  (assoc (into {}
               (for [type [:linear :polynomial :cubic-spline]]
                 [type #(interpolate % type)]))
    :b-spline (fn [points]
                (let [min (first (first points))
                      max (first (last points))
                      approximator (approximate (map second points))]
                  (fn [x]
                    (approximator (/ (- x min) (- max min))))))))

(defn get-plot-xs [points-xs]
  (let [min (first points-xs)
        max (last points-xs)]
    (->> (range min max (/ (- max min) 500))
         (concat points-xs)
         sort)))

(defn indexed [coll]
  (map-indexed vector coll))

(defn recalculate-curves [points parametric?]
  (when (> (count points) 1)
    (let [points (if parametric? (indexed points) (sort-by first points))
          xs (get-plot-xs (map first points))]
      (swap! curves empty)
      (doseq [type (keys interpolators)]
        (when-not (and (= type :cubic-spline)
                       (< (count points) 3))
          (let [ps (map ((interpolators type) points) xs)
                ps (if parametric? ps (map vector xs ps))]
            (swap! curves assoc type (line-join-points ps))))))))

(defn points-changed [points]
  (future-cancel @work-future)
  (reset! work-future (future (recalculate-curves points (:parametric @checkboxes)))))

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
    (do (swap! checkboxes update-in [id] not)
        (when (= id :parametric)
          (points-changed @points)))
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

(defn draw-curve [lines]
  (dorun (map #(apply line %) lines)))

(defn draw-curves []
  (doseq [[type curve] @curves]
    (when (@checkboxes type)
      (apply stroke (colors type))
      (draw-curve curve))))

(defn draw []
  (background 255)
  (draw-curves)
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
   :mouse-dragged mouse-dragged)
  (add-watch points nil
             (fn [_ _ _ new-points]
               (points-changed new-points))))

#_(run)