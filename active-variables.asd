;;; -*- Mode: Lisp -*-

(defpackage :active-variables-system
  (:use :cl :asdf))
(in-package :active-variables-system)

(defsystem :active-variables
  :name "cl-active-variables"
  :author "Burton Samograd <burton.samograd@gmail.com>"
  :version "1.0"
  :maintainer "Burton Samograd <burton.samograd@gmail.com>"
  :license "GNU General Public License v3"
  :description "Defines defactive/setactive, which is used for defining variables which have callbacks when read or written."
  :serial t
  :components ((:file "active-variables")))

