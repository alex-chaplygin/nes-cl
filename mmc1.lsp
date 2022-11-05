(defpackage :mmc1
  (:use :cl)
  (:export :wrt))
(in-package :mmc1)

(defvar reg #x10)
(defvar control 0)

(defun wrt (a d)
  (if (= #x80 (logand #x80 d)) (reset-reg)
      (if (= 1 (logand reg 1))
	  (progn
	    (setf reg (ash reg -1))
	    (switch a (set5bit (bit1 d))))
	  (progn
	    (setf reg (ash reg -1))
	    (set5bit (bit1 d))))))

(defun bit1 (d) (logand d 1))

(defun set5bit (bit)
  (setf reg (logior reg (ash bit 4))))
  
(defun reset-reg ()
  (progn
    (setf reg #x10)
    (setf control (logior control #xC))))

(defparameter *sw-table* ())
(defun sw (up func)
  (setf *sw-table* (append *sw-table* (list (cons up func)))))
  
(defun switch (a num)
  (defun f (lst)
    (if (null lst)
	(error "cannot switch mmc1")
	(let ((el (car lst)))
	  (if (< a (car el))
	      (funcall (cdr el) num)
	      (f (cdr lst))))))
  (f *sw-table*))

(defun mmc1-control (d)
  (list d :control))

(defun mmc1-chr-bank0 (d)
  (list d :chr0))

(defun mmc1-chr-bank1 (d)
  (list d :chr1))

(defun mmc1-prg-bank (d)
  (list d :prg))

(sw #xa000 #'mmc1-control)
(sw #xc000 #'mmc1-chr-bank0)
(sw #xe000 #'mmc1-chr-bank1)
(sw #x10000 #'mmc1-prg-bank)
