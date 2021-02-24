;;;; This file is part of convert-coordinates
;;;; Copyright 2021 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

#+(and sbcl sb-core-compression)
(defmethod perform ((o program-op) (c system))
  (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem "convert-coordinates"
  :name "convert-coordinates"
  :description "Geographic coordinates converter"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("maidenhead"
               "mcclim"
               "mgrs"
               "olc"
               "utm-ups")
  :build-operation "program-op"
  :build-pathname "convert-coordinates"
  :entry-point "convert-coordinates:gui"
  :components ((:file "convert-coordinates")))
