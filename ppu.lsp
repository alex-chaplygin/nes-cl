(defpackage :ppu
  (:use :cl)
  (:export :rd :wrt))

(in-package :ppu)

(defun rd (a) 'ppuread)
(defun wrt (a v) 'ppuwrite)
