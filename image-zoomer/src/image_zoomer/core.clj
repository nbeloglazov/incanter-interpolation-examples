(ns image-zoomer.core
  (:require [seesaw.core :refer :all]
            [seesaw.mig :refer :all]
            [seesaw.chooser :refer (choose-file)]
            [incanter.interpolation :refer (interpolate-grid approximate-grid)]))

(def image (atom nil))
(def interpolation-type (atom :bilinear))

(def radio-group (button-group))
(declare root)

(defn open-image []
  (choose-file
   :type :open
   :multi? false
   :filters [["Images" ["jpg" "jpeg" "bmp" "png"]]]
   :remember-directory? true))

(defn set-image [file]
  (when-not (nil? file)
    (reset! image (javax.imageio.ImageIO/read file))
    (println @image)))

(defn revalidate [frame]
  (doto (.. frame getContentPane)
    .revalidate
    .repaint))

(defn fit-image [comp image]
  (config! comp :preferred-size [(.getWidth image) :by (.getHeight image)]))

(defn open-button []
  (button
   :text "Open"
   :listen [:action (fn [_]
                      (set-image (open-image))
                      (fit-image (select root [:#image]) @image)
                      (revalidate root))]))

(defn zoom-button [n]
  (button :text (str n "x")))

(defn interp-type-button [name type]
  (radio :text name
         :listen [:selection (fn [_] (reset! interpolation-type type))]
         :selected? (= @interpolation-type type)
         :group radio-group))

(defn draw-image [comp gr image]
  (when-not (nil? image)
    (.drawImage gr image 0 0 nil)))

(defn get-layout []
  (mig-panel
   :constraints ["" "[grow][grow][grow][grow][grow]" ""]
   :id :layout
   :items [[(open-button)]
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
    (def root (-> (frame :title "Hello",
                       :content (get-layout)
                       :on-close :dispose)
                pack!
                show!))))

(-main)




