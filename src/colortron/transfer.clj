;; Henry Cooney <hacoo36@gmail.com> <Github: hacoo>
;; 24 Jan. 2015
;; colortron/transfer.clj

;; Does color transfer on OpenCV nested image matrics; based on 
;; 'Color Transfer Between Images' by
;; Erik Reinhard, Michael Ashikhmin, Bruce Gooch, Peter Shirley
;; Applied Perception 2001

;; Images should be in LaB form.

(ns colortron.transfer
  (:use (incanter core stats))
  (:require [clojure.string :refer [split join]]
            [colortron.utils :refer :all]
            [colortron.stats :refer :all])
  (:gen-class))

(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)

(import '(org.opencv.core Point Rect Size Mat CvType Scalar Range
                          MatOfInt)
        '(org.opencv.videoio VideoCapture VideoWriter)
        '(org.opencv.imgcodecs Imgcodecs)
        '(org.opencv.imgproc Imgproc))

(defn extract-channel
  "[[[num]]] -> int -> [num]
  Extract all values for a particular channel into a single vector"
  [image chan]
  (into [] (flatten (map #(map (fn [x] (nth x chan)) %) image))))

(defn combine-channels
  "int -> int -> &[num] -> [[[num]]]
  Combines vector of single channel elements into a nested image matrix
  with cols columns"
  [cols & chans]
   (let [zipped (apply (partial map vector) chans)]
    (loop [zs zipped
           image (transient [])]
      (if (empty? zs) (persistent! image)
          (recur (drop cols zs) 
                 (conj! image (into [] (take cols zs))))))))

(defn subtract-means
  "{:l [num] :a [num] :b [num]} -> {:l [num] :a [num] :b [num]}
  Subtract means of target from target and return"
  [tar]
  (let [lm (mean (:l tar))
        am (mean (:a tar))
        bm (mean (:b tar))]
    {:l (map #(- % lm) (:l tar))
     :a (map #(- % am) (:a tar))
     :b (map #(- % bm) (:b tar))}))

(defn scale-sds
  "{:l [num] :a [num] :b [num]} -> {:l [num] :a [num] :b [num]} ->
  {:l [num] :a [num] :b [num]}
  Scale tar to have the standard deviions of src"
  [src tar]
  {:l (map #(* % (/ (sd (:l tar)) (sd (:l src))) (:l tar)))
   :a (map #(* % (/ (sd (:a tar)) (sd (:a src))) (:a tar)))
   :b (map #(* % (/ (sd (:b tar)) (sd (:b src))) (:b tar)))})

(defn transfer-element
  "Transfer one color element"
  [tar sm tm ssd tsd]
  (+ (* (- tar tm) (/ ssd tsd)) sm))

(defn transfer-channel
  "Transfer a whole channel"
  [tar sm tm ssd tsd]
  (map #(transfer-element % sm tm ssd tsd) tar))

(defn transfer-colors
  "[[[num]]] -> [[[num]]] -> [[[num]]]
  Transfers color properties from source onto target, and 
  returns the modified Mat."
  [source target]
  (let [src {:l (extract-channel source 0) 
             :a (extract-channel source 1) 
             :b (extract-channel source 2)}
        tar {:l (extract-channel target 0) 
             :a (extract-channel target 1) 
             :b (extract-channel target 2)}
        src-means {:l (mean (:l src))
                   :a (mean (:a src))
                   :b (mean (:b src))}
        tar-means {:l (mean (:l tar))
                   :a (mean (:a tar))
                   :b (mean (:b tar))}
        src-sd {:l (sd (:l src))
                :a (sd (:a src))
                :b (sd (:b src))}
        tar-sd {:l (sd (:l tar))
                :a (sd (:a tar))
                :b (sd (:b tar))}]
    (combine-channels (count (first target))
                       (transfer-channel (:l tar) 
                                         (:l src-means)
                                         (:l tar-means) 
                                         (:l src-sd)
                                         (:l tar-sd))
                       (transfer-channel (:a tar) 
                                         (:a src-means)
                                         (:a tar-means) 
                                         (:a src-sd)
                                         (:a tar-sd))
                       (transfer-channel (:b tar) 
                                         (:b src-means)
                                         (:b tar-means) 
                                         (:b src-sd)
                                         (:b tar-sd)))))

;; Some test code for above
;; (def day (mat-to-vec (convert-matrix
;;           (Imgcodecs/imread "resources/paper_day.jpg")
;;           :rgb :lab)))
;; (def sun (mat-to-vec (convert-matrix
;;           (Imgcodecs/imread "resources/paper_sunset.jpg")
;;           :rgb :lab)))
;; (def paper (mat-to-vec (convert-matrix
;;           (Imgcodecs/imread "resources/paper_redocean.jpg")
;;           :rgb :lab)))
;; (def result (mat-to-vec (convert-matrix
;;           (Imgcodecs/imread "resources/transfer.jpg")
;;           :rgb :lab)))


;; (def out (transfer-colors sun day))
;; (sd (convert-matrix :rgb :lab (extract-channel sun 2)))
;; (sd (extract-channel sun 0))
;; (sd (extract-channel out 0))
;; (sd (extract-channel paper 0))


(defn mat-transfer-colors
  "Mat -> Mat -> Mat
  Transfer color stats from source onto target"
  [^Mat source ^Mat target]
  (vec3d-to-mat (transfer-colors 
                 (mat-to-vec source) (mat-to-vec target))))

(defn convert-matrix
  "Mat -> :rgb|:lab|:luv|:hsv -> :rgb|:lab|:luv|:hsv -> Mat
  Convert a mat from format 'from' to format 'to'" 
  [mat from to]
  (let [newmat (.clone mat)]
    (cond (= from to) newmat
          (= from :rgb)
          (cond (= to :lab) (Imgproc/cvtColor mat newmat Imgproc/COLOR_BGR2Lab)
                (= to :luv) (Imgproc/cvtColor mat newmat Imgproc/COLOR_BGR2Luv)
                (= to :hsv) (Imgproc/cvtColor mat newmat Imgproc/COLOR_BGR2HSV)
                :else (throw (Exception. "Invalid conversion type!")))
          (= from :lab)
          (cond (= to :rgb) (Imgproc/cvtColor mat newmat Imgproc/COLOR_Lab2BGR)
                :else (throw (Exception. "Invalid conversion type!")))
          (= from :luv)
          (cond (= to :rgb) (Imgproc/cvtColor mat newmat Imgproc/COLOR_Luv2BGR)
                :else (throw (Exception. "Invalid conversion type!")))
          (= from :hsv)
          (cond (= to :rgb) (Imgproc/cvtColor mat newmat Imgproc/COLOR_HSV2BGR)
                :else (throw (Exception. "Invalid conversion type!")))
          :else (throw (Exception. "Invalid conversion type!")))
    newmat))
          
(defn image-transfer-colors 
  "string -> string -> string -> :rgb|:lab -> 
  true|false File IO! (writes out image)
  Load images at tar-path and src-path. Assumes it is in RGB a
  and converts it to format fmt. Transfer the color statistics 
  from source onto target and write out the result at out-path 
  (in RGB)."
  [src-path tar-path out-path fmt]
  (let [src (convert-matrix (Imgcodecs/imread src-path) :rgb fmt)
        tar (convert-matrix (Imgcodecs/imread tar-path) :rgb fmt)
        output-mat (convert-matrix (mat-transfer-colors src tar)
                    fmt :rgb)]
    (Imgcodecs/imwrite out-path output-mat)))
;;(MatOfInt. 
;;(int-array Imgcodecs/CV_IMWRITE_JPEG_QUALITY)))))
    
(image-transfer-colors
 "resources/ocean_day.jpg"
 "resources/ocean_sunset.jpg"
 "resources/transfer.jpg" :rgb)

(javax.swing.ImageIcon. "resources/transfer.jpg")
