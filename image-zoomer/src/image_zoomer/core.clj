(ns image-zoomer.core
  (:require [seesaw.core :refer :all]
            [seesaw.mig :refer :all]
            [seesaw.chooser :refer (choose-file)]
            [incanter.interpolation :refer (interpolate-grid approximate-grid)]
            [clojure.java.io :refer (resource)]))

(def image (atom nil))
(def image-name (atom "nature.jpg"))
(def interpolation-type (atom :bilinear))

(def radio-group (button-group))
(def image-group (button-group))

(def root (atom nil))

(add-watch image-name :title-changed
           (fn [_ _ _ name]
             (when-let [root @root]
               (config! root :title name))))

(defn open-image []
  (choose-file
   :type :open
   :multi? false
   :filters [["Images" ["jpg" "jpeg" "bmp" "png"]]]
   :remember-directory? true))

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

(defn open-button []
  (button
   :text "Custom image"
   :listen [:action (fn [_]
                      (when-let [image (open-image)]
                        (.clearSelection image-group)
                        (set-image image)
                        (reset! image-name (.getName image))))]))

(defn zoom-button [n]
  (button :text (str n "x")))

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

(defn draw-image [comp gr image]
  (when-not (nil? image)
    (.drawImage gr image 0 0 nil)))

(defn get-layout []
  (mig-panel
   :constraints ["" "[grow][grow][grow][grow]" ""]
   :id :layout
   :items [[(builtin-image-button "Nature" "nature.jpg")]
           [(builtin-image-button "Sky" "sky.png")]
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
    (set-image nil)))

(-main)




