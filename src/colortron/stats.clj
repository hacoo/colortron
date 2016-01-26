;; Henry Cooney <hacoo36@gmail.com> <Github: hacoo>
;; 24 Jan. 2015
;; colortron/stats.clj

;; Functions for doing statistics on nested OpenCL image vectors

(ns colortron.stats
  (:use (incanter core stats datasets io optimize))
  (:require [clojure.string :refer [split join]]
            [colortron.utils :refer :all])
  (:import (org.opencv.core Point Rect Size Mat CvType Scalar
                            Range)
           (org.opencv.videoio VideoCapture VideoWriter)
           (org.opencv.imgcodecs Imgcodecs)
           (org.opencv.imgproc Imgproc))
  (:gen-class))

(defn channel-mean
  "[[[num]]] -> int -> num
  Take the mean of an image matrix, by channel"
  [image chan]
  (let [vals (flatten (map #(map (fn [x] (nth x chan)) %) image))]
    (mean vals)))

(defn channel-sd
  "[[[num]]] -> int -> num
  Take sd of an image matrix by channel"
  [image chan]
    (let [vals (flatten (map #(map (fn [x] (nth x chan)) %) image))]
    (sd vals)))
  







