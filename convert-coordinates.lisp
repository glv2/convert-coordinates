;;;; This file is part of convert-coordinates
;;;; Copyright 2021 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :convert-coordinates
  (:use :cl)
  (:export #:gui))

(in-package :convert-coordinates)


(defparameter *map* (asdf:system-relative-pathname "convert-coordinates"
                                                   "map.png"))

(clim:define-application-frame convert-coordinates ()
  ((world-map :initarg world-map
              :accessor world-map))
  (:menu-bar nil)
  (:panes
   (map :application-pane
        :min-width 1200 :max-width 1200
        :min-height 930 :max-height 930
        :display-time nil
        :display-function #'display-map)
   (lat/lon-1 :text-field
              :value "48.86 2.34"
              :activate-callback (lambda (pane) (update :lat/lon pane)))
   (lat/lon-deg-1 :text-field
                  :editable-p nil)
   (utm-1 :text-field
          :activate-callback (lambda (pane) (update :utm pane)))
   (mgrs-1 :text-field
           :activate-callback (lambda (pane) (update :mgrs pane)))
   (maidenhead-1 :text-field
                 :activate-callback (lambda (pane) (update :maidenhead pane)))
   (olc-1 :text-field
          :activate-callback (lambda (pane) (update :olc pane)))
   (lat/lon-2 :text-field
              :value "40°40'12\"N 73°56'24\"W"
              :activate-callback (lambda (pane) (update :lat/lon pane)))
   (lat/lon-deg-2 :text-field
                  :editable-p nil)
   (utm-2 :text-field
          :activate-callback (lambda (pane) (update :utm pane)))
   (mgrs-2 :text-field
           :activate-callback (lambda (pane) (update :mgrs pane)))
   (maidenhead-2 :text-field
                 :activate-callback (lambda (pane) (update :maidenhead pane)))
   (olc-2 :text-field
          :activate-callback (lambda (pane) (update :olc pane)))
   (distance :text-field
             :editable-p nil))
  (:layouts
   (default (clim:horizontally ()
              (clim:vertically ()
                (clim:labelling (:label "Point 1 (red)" :label-alignment :top)
                  (clim:vertically ()
                    (clim:labelling (:label "Latitude Longitude"
                                     :label-alignment :top)
                      (clim:vertically ()
                        lat/lon-1
                        lat/lon-deg-1))
                    (clim:labelling (:label "UTM/UPS" :label-alignment :top)
                      utm-1)
                    (clim:labelling (:label "MGRS" :label-alignment :top)
                      mgrs-1)
                    (clim:labelling (:label "Maidenhead" :label-alignment :top)
                      maidenhead-1)
                    (clim:labelling (:label "Open Location Code"
                                     :label-alignment :top)
                      olc-1)))
                (clim:labelling (:label "Point 2 (yellow)"
                                 :label-alignment :top)
                  (clim:vertically ()
                    (clim:labelling (:label "Latitude Longitude"
                                     :label-alignment :top)
                      (clim:vertically ()
                        lat/lon-2
                        lat/lon-deg-2))
                    (clim:labelling (:label "UTM/UPS" :label-alignment :top)
                      utm-2)
                    (clim:labelling (:label "MGRS" :label-alignment :top)
                      mgrs-2)
                    (clim:labelling (:label "Maidenhead" :label-alignment :top)
                      maidenhead-2)
                    (clim:labelling (:label "Open Location Code"
                                     :label-alignment :top)
                      olc-2)))
                (clim:labelling (:label "Distance" :label-alignment :top)
                  distance))
              (clim:labelling (:label "World map (Mercator projection)"
                               :label-alignment :top)
                map)))))

(defmethod initialize-instance :after ((instance convert-coordinates) &key)
  (setf (world-map instance) (clim:make-pattern-from-bitmap-file *map*)))

(defun display-map (frame pane)
  (clim:updating-output (pane)
    (let* ((image (world-map frame)))
           ;; (width (clim:pattern-width image))
           ;; (height (clim:pattern-height image))
           ;; (pane-width (clim:bounding-rectangle-width pane))
           ;; (pane-height (clim:bounding-rectangle-width pane))
      ;;      (scale (min (/ pane-width width) (/ pane-height height)))
      ;;      (scaling (clim:make-scaling-transformation scale scale)))
      ;; (clim:with-drawing-options (pane :transformation scaling)
        (clim:draw-design pane image))))

(defun get-lat/lon (source lat/lon utm mgrs maidenhead olc)
  (handler-case
      (ecase source
        ((:lat/lon)
         (utm-ups:parse-lat/lon (clim:gadget-value lat/lon)))
        ((:utm)
         (destructuring-bind (zone easting northing)
             (utm-ups:parse-utm/ups (clim:gadget-value utm))
           (utm-ups:utm/ups->lat/lon zone easting northing)))
        ((:mgrs)
         (mgrs:mgrs->lat/lon (clim:gadget-value mgrs)))
        ((:maidenhead)
         (maidenhead:maidenhead->lat/lon (clim:gadget-value maidenhead)))
        ((:olc)
         (olc:olc->lat/lon (clim:gadget-value olc))))
    (error () '(0 0))))

(defun update-coordinates (coordinates lat/lon lat/lon-deg utm mgrs maidenhead olc)
  (destructuring-bind (latitude longitude) coordinates
    (setf (clim:gadget-value lat/lon)
          (utm-ups:format-lat/lon latitude longitude t))
    (setf (clim:gadget-value lat/lon-deg)
          (utm-ups:format-lat/lon latitude longitude nil))
    (multiple-value-bind (coordinates band)
        (utm-ups:lat/lon->utm/ups latitude longitude)
      (destructuring-bind (zone easting northing) coordinates
        (setf (clim:gadget-value utm)
              (utm-ups:format-utm/ups zone easting northing band))))
    (setf (clim:gadget-value mgrs) (mgrs:lat/lon->mgrs latitude longitude))
    (setf (clim:gadget-value maidenhead)
          (maidenhead:lat/lon->maidenhead latitude longitude))
    (setf (clim:gadget-value olc) (olc:lat/lon->olc latitude longitude))))

(defun mercator (latitude)
  (log (tan (+ (/ (* pi (/ latitude 180)) 2) (/ pi 4)))))

(defun draw-point (coordinates map color thickness)
  (destructuring-bind (latitude longitude) coordinates
    (when (< -80 latitude 80)
      (let* ((image (world-map (clim:pane-frame map)))
             (width (clim:pattern-width image))
             (height (clim:pattern-height image))
             (center-x (/ width 2))
             (center-y (/ height 2))
             (x (+ center-x (* (/ longitude 360) width)))
             (y (- center-y  (* (/ (mercator latitude) (mercator 80) 2)
                                height))))
        (clim:draw-point* map x y :ink color :line-thickness thickness)))))

(defun distance (coordinates-1 coordinates-2)
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let* ((lat-1 (* latitude-1 pi 1/180))
             (lon-1 (* longitude-1 pi 1/180))
             (lat-2 (* latitude-2 pi 1/180))
             (lon-2 (* longitude-2 pi 1/180))
             (c (+ (* (cos lat-1) (cos lat-2) (cos (- lon-2 lon-1)))
                   (* (sin lat-1) (sin lat-2)))))
        (acos c)))))

(defun azimuth (coordinates-1 coordinates-2 &optional distance)
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (destructuring-bind (latitude-2 longitude-2) coordinates-2
      (let* ((lat-1 (* latitude-1 pi 1/180))
             (lon-1 (* longitude-1 pi 1/180))
             (lat-2 (* latitude-2 pi 1/180))
             (lon-2 (* longitude-2 pi 1/180))
             (d (or distance (distance coordinates-1 coordinates-2))))
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
             (mod (* R0 (/ 180 pi)) 360))))))))

(defun draw-path (coordinates-1 coordinates-2 map color &optional (steps 1000))
  (flet ((rad (x)
           (* x pi 1/180))
         (deg (x)
           (* x (/ 180 pi))))
    (destructuring-bind (latitude-1 longitude-1) coordinates-1
      (do ((step (/ (distance coordinates-1 coordinates-2) steps))
           (dlat 0 (* step (cos a)))
           (lat (rad latitude-1) (+ lat dlat))
           (dlon 0 (* step (/ (sin a) (cos lat))))
           (lon (rad longitude-1) (+ lon dlon))
           (a (rad (azimuth coordinates-1 coordinates-2))
              (rad (azimuth (list (deg lat) (deg lon)) coordinates-2)))
           (i 0 (1+ i)))
          ((> i steps))
        (draw-point (list (deg lat) (deg lon)) map color 2)))))

(defun update (source pane)
  (let* ((frame (clim:pane-frame pane))
         (map (clim:find-pane-named frame 'map))
         (lat/lon-1 (clim:find-pane-named frame 'lat/lon-1))
         (lat/lon-deg-1 (clim:find-pane-named frame 'lat/lon-deg-1))
         (utm-1 (clim:find-pane-named frame 'utm-1))
         (mgrs-1 (clim:find-pane-named frame 'mgrs-1))
         (maidenhead-1 (clim:find-pane-named frame 'maidenhead-1))
         (olc-1 (clim:find-pane-named frame 'olc-1))
         (coordinates-1 (get-lat/lon source lat/lon-1 utm-1 mgrs-1
                                     maidenhead-1 olc-1))
         (lat/lon-2 (clim:find-pane-named frame 'lat/lon-2))
         (lat/lon-deg-2 (clim:find-pane-named frame 'lat/lon-deg-2))
         (utm-2 (clim:find-pane-named frame 'utm-2))
         (mgrs-2 (clim:find-pane-named frame 'mgrs-2))
         (maidenhead-2 (clim:find-pane-named frame 'maidenhead-2))
         (olc-2 (clim:find-pane-named frame 'olc-2))
         (coordinates-2 (get-lat/lon source lat/lon-2 utm-2 mgrs-2
                                     maidenhead-2 olc-2))
         (distance (clim:find-pane-named frame 'distance)))
    (display-map frame map)
    (draw-path coordinates-1 coordinates-2 map clim:+green+)
    (update-coordinates coordinates-1 lat/lon-1 lat/lon-deg-1 utm-1 mgrs-1
                        maidenhead-1 olc-1)
    (draw-point coordinates-1 map clim:+red+ 6)
    (update-coordinates coordinates-2 lat/lon-2 lat/lon-deg-2 utm-2 mgrs-2
                        maidenhead-2 olc-2)
    (draw-point coordinates-2 map clim:+yellow+ 6)
    (setf (clim:gadget-value distance)
          (format nil "~d km"
                  (round (* 6378137d0 (distance coordinates-1 coordinates-2))
                         1000)))
    (clim:redisplay-frame-panes frame)))

(defun gui ()
  (let ((frame (clim:make-application-frame 'convert-coordinates)))
    (clim:run-frame-top-level frame)))
