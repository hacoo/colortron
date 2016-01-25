;; Henry Cooney <hacoo36@gmail.com> <Github: hacoo>
;; 22 Jan. 2015
;; colortron/core.clj

;; Core for color transfer program

(ns colortron.core
  (:require [colortron.utils :refer :all])
  (:import (org.opencv.core Point Rect Size Mat CvType Scalar)
           (org.opencv.videoio VideoCapture VideoWriter)
           (org.opencv.imgcodecs Imgcodecs)
           (org.opencv.imgproc Imgproc))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)

(def m (Mat. 5 10 CvType/CV_8UC1 (Scalar. 0 0)))
(def mr1 (.row m 1))
(def mc5 (.col m 5))
(.setTo mr1 (Scalar. 5 1))
(.setTo mc5 (Scalar. 1 0))
(.size m)
(print (.dump m))


