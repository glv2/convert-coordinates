;;;; This file is part of convert-coordinates
;;;; Copyright 2023 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :convert-coordinates-cli
  (:use :cl)
  (:export #:cli))

(in-package :convert-coordinates-cli)


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
      (t
       (get-lat/lon string)))))

(defun cli ()
  (let ((interactive-p (not (listen))))
    (when interactive-p
      (write-line "Enter \"q\" as coordinates to quit.")
      (write-line "Enter \"?\" to get an example of supported coordinates."))
    (loop :do
      (let ((lat/lon (get-coordinates interactive-p)))
        (if (eql lat/lon :eof)
            (return)
            (when lat/lon
              (destructuring-bind (latitude longitude) lat/lon
                (print-coordinates latitude longitude))))))))
