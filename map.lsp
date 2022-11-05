(defpackage :map
  (:use :cl)
  (:export :wrt :set-mapper))
(in-package :map)

(defparameter *mapper* 0)
(defparameter *maps* (make-array 100))

(defun wrt (a v)
  (funcall (svref *maps* *mapper*) a v))

(defun set-mapper (m)
  (setf *mapper* m))

(defun mapper (num func)
  (setf (svref *maps* num) func))

(defun NROM (a d)
  (error "write to NROM"))

(mapper 0 #'NROM)
(mapper 1 #'MMC1:wrt)
