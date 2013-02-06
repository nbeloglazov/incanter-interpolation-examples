(ns image-zoomer.core
  (:require [seesaw.core :refer :all]
            [seesaw.mig :refer :all]
            [seesaw.chooser :refer (choose-file)]
            [incanter.interpolation :refer (interpolate-grid approximate-grid)]
            [clojure.java.io :refer (resource)])
  (:import java.awt.Color))

(def image (atom nil))
(def image-name (atom "sky.png"))
(def zoomed-images (atom {}))
(def interpolators (atom {}))
(def interpolation-type (atom :bilinear))

(def radio-group (button-group))
(def image-group (button-group))

(def queue (java.util.concurrent.LinkedBlockingDeque.))

(def root (atom nil))

(add-watch image-name :title-changed
           (fn [_ _ _ name]
             (when-let [root @root]
               (config! root :title name))))

(defn constrain [v mn mx]
  (max mn (min mx v)))

(defn to-hsb [rgb-int]
  (let [color (Color. rgb-int)] 
      (seq (Color/RGBtoHSB (.getRed color)
                           (.getGreen color)
                           (.getBlue color)
                           nil))))

(defn to-rgb [rgb-int]
  (let [color (Color. rgb-int)]
    [(.getRed color) (.getGreen color) (.getBlue color)]))

(defn hsb-to-rgb-int [hsb]
  (let [hsb (map float (map #(constrain % 0.0 1.0) hsb))]
   (Color/HSBtoRGB (first hsb) (second hsb) (last hsb))))

(defn rgb-to-rgb-int [rgb]
  (let [rgb (map int (map #(constrain % 0 255) rgb))]
   (.getRGB (Color. (first rgb) (second rgb) (last rgb)))))

(def converter {:to-coll to-rgb
                :from-coll rgb-to-rgb-int})

(defn to-grid [image]
  (let [n (.getHeight image)
        m (.getWidth image)
        conv (:to-coll converter)]
    (for [i (range n)]
      (for [j (range m)]
        (conv (.getRGB image j i))))))

(defn to-image [grid]
  (let [n (count grid)
        m (count (first grid))
        grid (mapv vec grid)
        conv (:from-coll converter)
        image (java.awt.image.BufferedImage. m n java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (println "Start to image" n m)
    (dotimes [i n]
      (println i)
      (dotimes [j m]
        (.setRGB image j i (conv (get-in grid [i j])))))
    (println "Stop to image" n m)
    image))

(defn vector-interpolator [grid interpolator]
  (let [grids (for [k (range 3)]
                (for [row grid]
                  (for [color row]
                    (nth color k))))
        interpolators (doall (map interpolator grids))]
    (fn [x y]
      (map #(% x y) interpolators))))


(defn get-interpolator [image-name grid type]
  (let [key {:image image-name
             :type type}]
    (if (contains? @interpolators key)
      (@interpolators key)
      (let [pr (promise)
            n (count grid)
            m (count (first grid))]
        (swap! interpolators assoc key pr)
        (println "Calculate interpolator " image-name type)
        (deliver pr (vector-interpolator grid
                                         (if (= :b-spline type)
                                           #(approximate-grid %)
                                           #(interpolate-grid % type))))
        (println "Finished calculation interpolator " image-name type)
        pr))))

(defn zoom-grid [interpolate n width height]
  (let [coef-x (/ 1.0 (dec width))
        coef-y (/ 1.0 (dec height))]
    (let [res (doall (for [i (range height)]
                       (do (println i)
                           (doall (for [j (range width)]
                                    (doall (interpolate (* j coef-x) (* i coef-y))))))))]
      (println "Calculated grid")
      res)))


(defn process-job [queue]
  (let [{:keys [image n type promise name]} (.take queue)]
    (println "Start" type n)
    (let [grid (to-grid image)
          interpolate @(get-interpolator name grid type)
          grid (zoom-grid interpolate n (* n (count grid)) (* n (count (first grid))))
          _ (println "Calculated")
          image (to-image grid)]
      (println "Delivering")
      (deliver promise image))
    (println "Finish" type n)
    (process-job queue)))

(defn start-workers []
  (dotimes [_ (max 1 (dec (.. Runtime getRuntime availableProcessors)))]
    (future (process-job queue))))

(defn open-image []
  (choose-file
   :type :open
   :multi? false
   :filters [["Images" ["jpg" "jpeg" "bmp" "png"]]]
   :remember-directory? true))

(defn get-zoomed-image [n]
  (let [key {:name @image-name
             :n n
             :type @interpolation-type}]
    (when-not (@zoomed-images key)
      (let [pr (promise)]
        (println "Add job" key)
        (.put queue (assoc key :image @image :promise pr))
        (swap! zoomed-images assoc key pr)))
    (@zoomed-images key)))

(defn revalidate [frame]
  (doto (.. frame getContentPane)
    .revalidate
    .repaint)
  (.pack frame))

(defn fit-image [comp image]
  (config! comp :preferred-size [(.getWidth image) :by (.getHeight image)]))

(defn set-image [file]
  (when-not (nil? file)
    (reset! image (javax.imageio.ImageIO/read file)))
  (when-let [root @root]
    (fit-image (select root [:#image]) @image)
    (revalidate root)))

(defn draw-image [_ gr image]
  (if-not (nil? image)
    (.drawImage gr image 0 0 nil)
    (.drawString gr "Calculating..." 10 50)))

(defn show-image-in-window [title image-promise]
  (invoke-later
   (let [image (atom nil)
         panel (border-panel :preferred-size [100 :by 100]
                             :paint (fn [comp gr] (draw-image comp gr @image)))
         window (frame :title title
                       :on-close :dispose
                       :resizable? false
                       :content panel)]
     (-> window pack! show! (.setLocationRelativeTo nil))
     (future (reset! image @image-promise)
             (fit-image panel @image)
             (revalidate window)))))

(defn open-button []
  (button
   :text "Custom image"
   :listen [:action (fn [_]
                      (when-let [image (open-image)]
                        (.clearSelection image-group)
                        (set-image image)
                        (reset! image-name (.getName image))))]))

(defn zoom-button [n]
  (button :text (str n "x")
          :listen [:action (fn [_]
                             (show-image-in-window (str @image-name " " n "x " (name @interpolation-type))
                                                   (get-zoomed-image n)))]))

(defn interp-type-button [name type]
  (radio :text name
         :listen [:selection (fn [_] (reset! interpolation-type type))]
         :selected? (= @interpolation-type type)
         :group radio-group))

(defn builtin-image-button [name image]
  (radio :text name
         :listen [:selection (fn [_]
                               (set-image (resource image))
                               (reset! image-name image))]
         :selected? (= image @image-name)
         :group image-group))

(defn get-layout []
  (mig-panel
   :constraints ["" "[grow][grow][grow][grow]" ""]
   :id :layout
   :items [[(builtin-image-button "Sky" "sky.png")]
           [(builtin-image-button "Nature" "nature.jpg")]
           [(builtin-image-button "Lenna" "lenna.png")]
           [(open-button) "wrap"]
           [(zoom-button 1)]
           [(zoom-button 2)]
           [(zoom-button 4)]
           [(zoom-button 8) "wrap"]
           [(interp-type-button "Bilinear" :bilinear)]
           [(interp-type-button "Polynomial" :polynomial)]
           [(interp-type-button "Bicubic spline" :bicubic-spline)]
           [(interp-type-button "B-spline" :b-spline) "wrap"]
           [(border-panel :preferred-size [100 :by 100]
                          :paint (fn [comp gr] (draw-image comp gr @image))
                          :id :image) "span"]]))


(defn -main [& args]
  (invoke-later
    (reset! root (-> (frame :title @image-name,
                            :content (get-layout)
                            :on-close :dispose
                            :resizable? false)
                     pack!
                     show!))
    (.setLocationRelativeTo @root nil)
    (set-image nil))
  (start-workers))

(-main)




