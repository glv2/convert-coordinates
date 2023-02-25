;;;; This file is part of convert-coordinates
;;;; Copyright 2023 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :convert-coordinates-cli
  (:use :cl)
  (:export #:cli))

(in-package :convert-coordinates-cli)


(defparameter *point-1* nil)
(defparameter *point-2* nil)

(defconstant +earth-radius+ 6378137d0
  "Radius of the planet")

(defun rad (x)
  "Convert the X angle from degrees to radians."
  (* x pi 1/180))

(defun deg (x)
  "Convert the X angle from radians to degrees."
  (* x (/ 180 pi)))

(defun wrap-longitude (lon)
  "Wrap the longitude angle so that it stays between -π and π."
  (cond
    ((< lon (- pi))
     (+ lon (* 2 pi)))
    ((> lon pi)
     (- lon (* 2 pi)))
    (t
     lon)))

(defun mercator (lat)
  "Compute the Mercator projection y for a latitude."
  (log (tan (+ (/ lat 2) (/ pi 4)))))

(defun distance-orthodrome (coordinates-1 coordinates-2)
  "Compute the distance between COORDINATES-1 and COORDINATES-2 on a great
circle."
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let* ((lat-1 (rad latitude-1))
             (lon-1 (rad longitude-1))
             (lat-2 (rad latitude-2))
             (lon-2 (rad longitude-2))
             (c (+ (* (cos lat-1) (cos lat-2) (cos (- lon-2 lon-1)))
                   (* (sin lat-1) (sin lat-2)))))
        (acos c)))))

(defun azimuth-orthodrome (coordinates-1 coordinates-2 &optional distance)
  "Compute the initial azimuth to follow to go from COORDINATES-1 to
COORDINATES-2 on a great circle. If DISTANCE is not given, it is computed."
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let ((lat-1 (rad latitude-1))
            (lon-1 (rad longitude-1))
            (lat-2 (rad latitude-2))
            (lon-2 (rad longitude-2))
            (d (or distance
                   (distance-orthodrome coordinates-1 coordinates-2))))
        (cond
          ((zerop d)
           0)
          ((= lon-1 lon-2)
           (if (> lat-1 lat-2) 180 0))
          (t
           (let* ((sinR0 (/ (* (cos lat-2) (sin (- lon-2 lon-1))) (sin d)))
                  (cosR0 (/ (- (sin lat-2) (* (sin lat-1) (cos d)))
                            (* (cos lat-1) (sin d))))
                  (R0 (phase (complex cosR0 sinR0))))
             (mod (deg R0) 360))))))))

(defun azimuth-loxodrome (coordinates-1 coordinates-2)
  "Compute the azimuth to follow to go from COORDINATES-1 to COORDINATES-2 on
a rhumb line."
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let ((lat-1 (rad latitude-1))
            (lon-1 (rad longitude-1))
            (lat-2 (rad latitude-2))
            (lon-2 (rad longitude-2)))
        (if (= lat-1 lat-2)
            (if (> lon-2 lon-1) 90 270)
            (let* ((tanR0 (/ (wrap-longitude (- lon-2 lon-1))
                             (- (mercator lat-2) (mercator lat-1))))
                   (R0 (+ (atan tanR0) (if (< lat-2 lat-1) pi 0))))
              (mod (deg R0) 360)))))))

(defun distance-loxodrome (coordinates-1 coordinates-2 &optional azimuth)
  "Compute the distance between COORDINATES-1 and COORDINATES-2 on a rhumb
line. If AZIMUTH is not given, it is computed."
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let ((lat-1 (rad latitude-1))
            (lon-1 (rad longitude-1))
            (lat-2 (rad latitude-2))
            (lon-2 (rad longitude-2))
            (az (rad (or azimuth
                         (azimuth-loxodrome coordinates-1 coordinates-2)))))
        (if (= lat-1 lat-2)
            (* (abs (- lon-2 lon-1)) (cos lat-1))
            (/ (- lat-2 lat-1) (cos az)))))))

(defun print-coordinates (latitude longitude)
  (format t "~%~20@A~22T~A~%~22T~A~%"
          "Latitude Longitude"
          (utm-ups:format-lat/lon latitude longitude t)
          (utm-ups:format-lat/lon latitude longitude nil))
  (multiple-value-bind (coordinates band)
      (utm-ups:lat/lon->utm/ups latitude longitude)
    (destructuring-bind (zone easting northing) coordinates
      (format t "~20@A~22T~A~%"
              "UTM/UPS"
              (utm-ups:format-utm/ups zone easting northing band))))
  (format t "~20@A~22T~A~%"
          "MGRS"
          (mgrs:lat/lon->mgrs latitude longitude))
  (format t "~20@A~22T~A~%"
          "Maidenhead"
          (maidenhead:lat/lon->maidenhead latitude longitude))
  (format t "~20@A~22T~A~%"
          "Open Location Code"
          (olc:lat/lon->olc latitude longitude t))
  (finish-output))

(defun print-example ()
  (print-coordinates 48.86 2.34))

(defun print-distances (coordinates-1 coordinates-2)
  (let* ((d (distance-orthodrome coordinates-1 coordinates-2))
         (a (azimuth-orthodrome coordinates-1 coordinates-2 d)))
    (format t "~%~20@A~22T~A km~%~22T~A°~%"
            "Orthodrome"
            (round (* d +earth-radius+) 1000)
            (round a)))
  (let* ((a (azimuth-loxodrome coordinates-1 coordinates-2))
         (d (distance-loxodrome coordinates-1 coordinates-2 a)))
    (format t "~20@A~22T~A km~%~22T~A°~%"
            "Loxodrome"
            (round (* d +earth-radius+) 1000)
            (round a)))
  (finish-output))

(defun get-lat/lon (string)
  (or (ignore-errors (utm-ups:parse-lat/lon string))
      (ignore-errors (mgrs:mgrs->lat/lon string t))
      (ignore-errors (maidenhead:maidenhead->lat/lon string t))
      (ignore-errors (olc:olc->lat/lon string t))
      (ignore-errors (apply #'utm-ups:utm/ups->lat/lon
                            (utm-ups:parse-utm/ups string)))
      (when (plusp (length string))
        (format t "~%~20@A~22T~A~%" "Invalid coordinates" string)
        nil)))

(defun get-coordinates (interactive-p)
  (when interactive-p
    (format t "~%Coordinates: ")
    (finish-output))
  (let ((string (read-line *standard-input* nil :eof)))
    (cond
      ((or (eql string :eof) (string-equal string "q"))
       :eof)
      ((string-equal string "?")
       (print-example)
       nil)
      ((string-equal string "d")
       (print-distances *point-1* *point-2*)
       nil)
      (t
       (get-lat/lon string)))))

(defun cli ()
  (let ((*point-1* '(0 0))
        (*point-2* '(0 0))
        (interactive-p (not (listen))))
    (when interactive-p
      (write-line "Enter \"q\" as coordinates to quit.")
      (write-line "Enter \"?\" to get an example of supported coordinates.")
      (write-line "Enter \"d\" to get the distance between the last 2 entered points."))
    (loop :do
      (let ((lat/lon (get-coordinates interactive-p)))
        (if (eql lat/lon :eof)
            (return)
            (when lat/lon
              (setf *point-1* *point-2*)
              (setf *point-2* lat/lon)
              (destructuring-bind (latitude longitude) lat/lon
                (print-coordinates latitude longitude))))))))
