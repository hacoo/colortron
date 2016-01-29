4;; Henry Cooney <hacoo36@gmail.com> <Github: hacoo>
;; 22 Jan. 2015
;; colortron/core.clj

;; Seesaw gui for color transfer. A little jumbled,
;; wonder what a better way is to keep seesaw code organized?

(ns colortron.core
  (:use seesaw.core seesaw.font seesaw.chooser)
  (:require [colortron.utils :refer :all]
            [colortron.transfer :refer :all]
            [clojure.repl :refer [doc]]
  (:gen-class)))

(def transfer-out-path "resources/transfer.jpg")
(def f (frame :title "Transfer time!"))
(defn display [content] 
  (config! f :content content) 
  (-> f pack! show!))
(def rb-group (button-group))
(def rbs (for [i [:lab :rgb :luv :hsv]] 
           (radio :id i :class :type :text (name i) :group rb-group)))
(config! rb-group :buttons rbs)
(selection! rb-group (first rbs))

(def source-path (atom nil))
(def tar-path (atom nil))


(def source-button (button :text "Select source..."))
(def tar-button (button :text "Select target..."))
(def source-box (label :text "Source image" :icon nil))
                 
(def tar-box (label :text "Target image" :icon nil))
(def output-box (label :text "Output image" :icon nil))

(def splits (left-right-split (scrollable source-box)
                              (left-right-split
                               (scrollable tar-box)
                               (scrollable output-box))))
(def go-button (button :text "Transfer!"))

(def bp (border-panel
  :north (horizontal-panel 
          :items (concat rbs [source-button tar-button
                              go-button]))
  :center splits))

(defn choose-image 
  "Choose an image file, return the path"
  []
  (when-let [f (choose-file :type :open :dir "resources/")]
    (.getPath f)))

(listen source-button :action (fn [x]
                                (try 
                                  (when-let [path (choose-image)]
                                    (reset! source-path path)
                                    (config! source-box :icon 
                                             (javax.imageio.ImageIO/read
                                              (java.io.File. path)))
                                    (pack! f))
                                (catch Exception e (println e)))))

(listen tar-button :action (fn [x]
                                (try 
                                  (when-let [path (choose-image)]
                                    (reset! tar-path path)
                                    (config! tar-box :icon 
                                             (javax.imageio.ImageIO/read
                                              (java.io.File. path))) 
                                    (pack! f))
                                (catch Exception e (println e)))))

(listen go-button :action (fn [x]
                            (try
                              (if-let [src @source-path]
                                (if-let [tar @tar-path]
                                  (if-let [sel (selection rb-group)]
                                    (if-let [output 
                                             (image-transfer-colors
                                              src
                                              tar
                                              transfer-out-path
                                              (keyword (text sel)))]
                                      (do 
                                        (println "Transfering "
                                                 src
                                                 "onto "
                                                 tar "...")
                                        (config! output-box :icon
                                          (javax.imageio.ImageIO/read
                                           (java.io.File.
                                            transfer-out-path)))
                                        (pack! f))
                                     (println "Error - Tranfer failed."))
                                    (println "Error - No type selected"))
                                (println "Error - No target selected"))
                                (println "Error - No source selected"))
                              (catch Exception e (println e)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (display bp))



