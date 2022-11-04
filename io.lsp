(defpackage :io
  (:use :cl)
  (:export :rd :wrt))
(in-package :io)

(defun rd (a) 'ioread)
(defun wrt (a v) 'iowrite)
