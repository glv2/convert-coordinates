;;;; This file is part of convert-coordinates
;;;; Copyright 2021 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :convert-coordinates
  (:use :cl)
  (:export #:gui))

(in-package :convert-coordinates)


(defconstant +earth-radius+ 6378137d0)
(defparameter *map* (asdf:system-relative-pathname "convert-coordinates"
                                                   "map.png"))

(clim:define-application-frame convert-coordinates ()
  ((world-map :accessor world-map
              :initform (clim:make-pattern-from-bitmap-file *map*)))
  (:menu-bar nil)
  (:panes
   (map :application-pane
        :min-width 1200 :max-width 1200
        :min-height 930 :max-height 930
        :display-time nil
        :display-function #'clear-map)
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
              :value "45°31'12.3\"N 122°40'45.6\"W"
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
   (distance-ortho :text-field
                   :editable-p nil)
   (azimuth-ortho :text-field
                  :editable-p nil)
   (distance-loxo :text-field
                  :editable-p nil)
   (azimuth-loxo :text-field
                 :editable-p nil))
  (:layouts
   (default
    (clim:horizontally ()
      (1/4 (clim:vertically ()
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
             (clim:labelling (:label "Orthodrome (orange)" :label-alignment :top)
               (clim:vertically ()
                 (clim:labelling (:label "Distance" :label-alignment :top)
                   distance-ortho)
                 (clim:labelling (:label "Initial azimuth"
                                  :label-alignment :top)
                   azimuth-ortho)))
             (clim:labelling (:label "Loxodrome (purple)" :label-alignment :top)
               (clim:vertically ()
                 (clim:labelling (:label "Distance" :label-alignment :top)
                   distance-loxo)
                 (clim:labelling (:label "Azimuth" :label-alignment :top)
                   azimuth-loxo)))))
      (3/4 (clim:labelling (:label "World map (Mercator projection)"
                            :label-alignment :top)
             map))))))

(defun clear-map (frame pane)
  (clim:window-clear pane)
  (clim:draw-design pane (world-map frame)))

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

(defun rad (x)
  (* x pi 1/180))

(defun deg (x)
  (* x (/ 180 pi)))

(defun wrap-longitude (lon)
  (cond
    ((< lon (- pi))
     (+ lon (* 2 pi)))
    ((> lon pi)
     (- lon (* 2 pi)))
    (t
     lon)))

(defun mercator (lat)
  (log (tan (+ (/ lat 2) (/ pi 4)))))

(defun draw-point (coordinates map color thickness)
  (destructuring-bind (latitude longitude) coordinates
    (when (< -80 latitude 80)
      (let* ((lat (rad latitude))
             (lon (rad longitude))
             (image (world-map (clim:pane-frame map)))
             (width (clim:pattern-width image))
             (height (clim:pattern-height image))
             (center-x (/ width 2))
             (center-y (/ height 2))
             (x (+ center-x (round (* (/ lon 2 pi) width))))
             (y (- center-y  (round (* (/ (mercator lat)
                                          (mercator (rad 80)) 2)
                                       height)))))
        (clim:draw-point* map x y :ink color :line-thickness thickness)))))

(defun distance-orthodrome (coordinates-1 coordinates-2)
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

(defun draw-orthodrome (coordinates-1 coordinates-2 map color)
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (let* ((distance (distance-orthodrome coordinates-1 coordinates-2))
           (step (/ 1000 +earth-radius+))
           (n (floor distance step)))
      (do ((lat (rad latitude-1) (+ lat dlat))
           (lon (rad longitude-1) (wrap-longitude (+ lon dlon)))
           (az (rad (azimuth-orthodrome coordinates-1 coordinates-2 distance))
               (rad (azimuth-orthodrome (list (deg lat) (deg lon))
                                        coordinates-2)))
           (dlat 0 (* step (cos az)))
           (dlon 0 (* step (/ (sin az) (cos lat))))
           (i 0 (1+ i)))
          ((> i n))
        (when (zerop (mod i 100))
          (draw-point (list (deg lat) (deg lon)) map color 3))))))

(defun draw-loxodrome (coordinates-1 coordinates-2 map color)
  (destructuring-bind (latitude-1 longitude-1) coordinates-1
    (let* ((azimuth (azimuth-loxodrome coordinates-1 coordinates-2))
           (distance (distance-loxodrome coordinates-1 coordinates-2 azimuth))
           (step (/ 1000 +earth-radius+))
           (n (floor distance step))
           (az (rad azimuth)))
      (do ((lat (rad latitude-1) (+ lat dlat))
           (lon (rad longitude-1) (wrap-longitude (+ lon dlon)))
           (dlat 0 (* step (cos az)))
           (dlon 0 (* step (/ (sin az) (cos lat))))
           (i 0 (1+ i)))
          ((> i n))
        (when (zerop (mod i 100))
          (draw-point (list (deg lat) (deg lon)) map color 3))))))

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
         (distance-ortho (clim:find-pane-named frame 'distance-ortho))
         (azimuth-ortho (clim:find-pane-named frame 'azimuth-ortho))
         (distance-loxo (clim:find-pane-named frame 'distance-loxo))
         (azimuth-loxo (clim:find-pane-named frame 'azimuth-loxo))
         (dist-ortho (distance-orthodrome coordinates-1 coordinates-2))
         (az-ortho (azimuth-orthodrome coordinates-1 coordinates-2 dist-ortho))
         (az-loxo (azimuth-loxodrome coordinates-1 coordinates-2))
         (dist-loxo (distance-loxodrome coordinates-1 coordinates-2)))
    (clear-map frame map)
    (draw-loxodrome coordinates-1 coordinates-2 map clim:+purple+)
    (draw-orthodrome coordinates-1 coordinates-2 map clim:+orange+)
    (update-coordinates coordinates-1 lat/lon-1 lat/lon-deg-1 utm-1 mgrs-1
                        maidenhead-1 olc-1)
    (draw-point coordinates-1 map clim:+red+ 6)
    (update-coordinates coordinates-2 lat/lon-2 lat/lon-deg-2 utm-2 mgrs-2
                        maidenhead-2 olc-2)
    (draw-point coordinates-2 map clim:+yellow+ 6)
    (setf (clim:gadget-value distance-ortho)
          (format nil "~d km" (round (* +earth-radius+ dist-ortho) 1000)))
    (setf (clim:gadget-value azimuth-ortho)
          (format nil "~d°" (round az-ortho)))
    (setf (clim:gadget-value distance-loxo)
          (format nil "~d km" (round (* +earth-radius+ dist-loxo) 1000)))
    (setf (clim:gadget-value azimuth-loxo)
          (format nil "~d°" (round az-loxo)))
    (clim:redisplay-frame-panes frame)))

(defun gui ()
  (let ((frame (clim:make-application-frame 'convert-coordinates)))
    (clim:run-frame-top-level frame)))
