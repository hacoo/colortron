;; Henry Cooney <hacoo36@gmail.com> <Github: hacoo>
;; 22 Jan. 2015
;; colortron/utils.clj

;; Opencv utiliy functions and wrappers

(ns colortron.utils
  (:use (incanter core))
  (:require [clojure.string :refer [split join]])
  (:import (org.opencv.core Point Rect Size Mat CvType Scalar
                            Range)
           (org.opencv.videoio VideoCapture VideoWriter)
           (org.opencv.imgcodecs Imgcodecs)
           (org.opencv.imgproc Imgproc))
  (:gen-class))

(defn col-seq
  "Mat -> [Mat]
  Takes a Mat and returns a vector of its columns."
  [^Mat mat]
  (loop [cols []
         numcols (.cols mat)
         i 0]
    (if (>= i numcols) cols
        (recur (conj cols (.col mat i)) numcols (inc i)))))

(defn chans-to-vec
  "OpenCV Matrix Element Thing -> [num]
  Turn an opencv array (or whatever it is) representing and n-channel
  matrix element into a vector."
  [element]
  (loop [new []
         old element]
    (if (empty? old) new 
        (recur (conj new (first old)) (rest old)))))

(defn row-seq
  "Mat -> [Mat]
  Takes a Mat and returns a vector of its rows."
  [^Mat mat]
  (loop [rows []
         numrows (.rows mat)
         i 0]
    (if (>= i numrows) rows
        (recur (conj rows (.row mat i)) numrows (inc i)))))

(defn row-to-vec
  "Mat -> [num]
  Take a matrix and dump first row contents to a vector."
  [^Mat mat]
  (loop [vals []
         max (.width (.size mat)) 
         i 0]
    (if (>= i max) vals
        (recur (conj vals (chans-to-vec (.get mat 0 i)))
               max (inc i)))))

(defn mat-to-vec
  "Mat -> [[[num]]]
  Turns a mat into a (cols x rows x channels) nested vector."
  [^Mat mat]
  (apply vector (map row-to-vec (row-seq mat))))

(defn pretty-2d-vec
  "[[A]] -> int -> String
  Make a pretty string for printing nested 2d-vectors."
  [vecs xmax]
  (if (< (count vecs) (* 2 xmax));; The whole row can be one string
    (str vecs)
    ;; otherwise, print only xmax first and last elements
  (let [first-n (join " " (take xmax vecs))       
        last-n (join " " (take-last xmax vecs))]
    (str "[" first-n " ... " last-n "]"))))

(defn pretty-3d-vec
  "[[[A]]] -> int -> int -> String
  Make a pretty string for printing nested 3d-vectors."
  [vecs xmax ymax]
  (let [vecstrings (map #(pretty-2d-vec % xmax) vecs)]
    (if (< (count vecs) ymax) (str "[" (join " \n " vecstrings) "]")
        (let [first-n (join " \n " (take ymax vecstrings))       
              last-n (join " \n " (take-last ymax vecstrings))]          
          (str "[" first-n "\n ... \n " last-n "]")))))

(defn print-mat
  "Mat -> nil IO!
  Prints a matrix in a nice & pretty way."
  [^Mat mat]
  (do
    (println "\n")
    (println (str " Type: " (.type mat)))
    (println (str " Dims: " (.dims mat)))
    (println (str "   Width: " (.width (.size mat))))
    (println (str "   Height: " (.height (.size mat))))
    (println (str "   Channels: " (.channels mat)))
    (println "\n")
    (print (pretty-3d-vec (mat-to-vec mat) 3 3))))
    
(defn vec2d-to-mat
  "[[num]] -> Mat
  Convert a 2d matrix row to a 1xn Mat."
  [vecs]
  (loop [m (Mat. 1 (count vecs) CvType/CV_8UC3)
         v vecs
         i 0]
    (if (empty? v) m
        (do
          (.put m 0 i (double-array (first v)))
          (recur m (rest v) (inc i)))))) 

(defn vec2d-into-mat-row
  "[[num]] -> Mat mutated! -> nil
  Sets row-matrix m to the values in vecs. m is mutated in place."
  [vecs m]
  (loop [v vecs
         i 0]
    (if (empty? v) nil
        (do
          (.put m 0 i (double-array (first v)))
          (recur (rest v) (inc i))))))

(defn vec3d-to-mat
  "[[[num]]] -> Mat
  Covert a nested 3d vector to a nxm Mat."
  [vecs]
  (loop [m (Mat. (count vecs) (count (first vecs)) CvType/CV_8UC3)
         vs vecs
         i 0]
    (if (empty? vs) m
        (do
          (vec2d-into-mat-row (first vs) (.row m i))
          (recur m (rest vs) (inc i))))))



;; Color conversion stuff

(defn log-each
  [image]
  (into [] (map #(Math/log10 %) image)))

(defn log-each-row
  [image]
  (into [] (map #(log-each %) image)))


(defn maplog
  [image]
  (into [] (map #(log-each-row %) image)))

(defn unlog-each
  [image]
  (into [] (map #(Math/pow 10 %) image)))

(defn unlog-each-row
  [image]
  (into [] (map #(unlog-each %) image)))


(defn mapunlog
  [image]
  (into [] (map #(unlog-each-row %) image)))
    



(defn multiply-each-row
  [row mat]
  (into []
        (map (fn [x] (into [] (mmult mat x))) row)))
  

(defn multiply-each-by-matrix 
  [image mat]
  (into [] (map #(multiply-each-row % mat) image)))
  

(defn rgb->xyz [image]
  (multiply-each-by-matrix image
              [[0.5141 0.3239 0.1604]
               [0.2651 0.6702 0.0641]
               [0.0241 0.1228 0.8444]]))

(defn xyz->lms [image]
  (multiply-each-by-matrix image
                           [[0.3897 0.6890 -0.0787]
                            [-0.2298 1.1834 0.0464]
                            [0.0 0.0 1.0]]))

(defn rgb->lms [image]
  (multiply-each-by-matrix image
                           [[0.3811 0.5783 0.0402]
                            [0.1967 0.7244 0.0782]
                            [0.0241 0.1288 0.8444]]))

(defn lms->LMS [image]
  (maplog image)) 

(defn LMS->lab [image]
  (multiply-each-by-matrix
   (multiply-each-by-matrix image
                           [[1.0 1.0 1.0]
                            [1.0 1.0 -2]
                            [1.0 -1.0 0]])
   [[0.5774 0.0 0.0 ]
    [0.0 0.4082 0.0]
    [0.0 0.0 0.7071]]))

(defn lab->LMS [image]
  (multiply-each-by-matrix
   (multiply-each-by-matrix image
                           [[0.5774 0.0 0.0]
                            [0.0 0.4082 0.0]
                            [0.0 0.0 0.7071]])
       [[1.0 1.0 1.0]
        [1.0 1.0 -1.0]
        [1.0 -2.0 0]]))

(defn LMS->lms [image]
  (mapunlog image))

(defn lms->rgb [image]
  (multiply-each-by-matrix image 
                           [[4.4679 -3.5873 0.1193]
                            [-1.2186 2.3809 -0.1624]
                            [0.0497 -0.2439 1.2045]]))
 
(defn convert-to-lab
  [image]
  (-> image rgb->lms lms->LMS LMS->lab))

(defn convert-from-lab
  [image]
  (-> image lab->LMS LMS->lms lms->rgb))
