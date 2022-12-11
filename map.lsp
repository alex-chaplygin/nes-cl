(defpackage :map
  (:use :cl)
  (:export :wrt :set-mapper :*mapper*))
(in-package :map)

(defparameter *mapper* 0)
(defparameter *maps* (make-array 100))

(defun wrt (a v)
 ; (format T "Write to mapper adr:~X val:~X~%" a v)
  (funcall (svref *maps* *mapper*) a v))

(defun set-mapper (m)
  (setf *mapper* m))

(defun mapper (num func)
  (setf (svref *maps* num) func))

(defun NROM (a d)
;  (format T "Write to mapper adr:~X val:~X~%" a v)
  (error "write to NROM") a d)

(defun UxROM (a d)
  (mem:write-bank1 (cart:get-prg (logand d #xF))) a)

(mapper 0 #'NROM)
(mapper 1 #'MMC1:wrt)
(mapper 2 #'UxROM)
