(defpackage :io
  (:use :cl)
  (:export :rd :wrt :*buttons*))
(in-package :io)

(defconstant +controller1+ #x4016) ;порт контроллера
(defconstant +controller2+ #x4017)
(defparameter *buttons* 0) ;состояние кнопок

(defun rd (a)
  "Чтение состояния кнопок по одному биту"
  ;(format t "IO:Read ~X:" a)
  (if (= a +controller1+)
    (let ((o (logand *buttons* 1)))
      (setf *buttons* (ash *buttons* -1))
;      (format t "~X~%" o)
      o) 0))

(defun wrt (a v)
  "Запись в порт, загрузка состояния кнопок"
  ;(format t "IO:Write ~X:~X~%" a v)
  (when (= a +controller1+)
    (when (= v 1)
      (setf *buttons* (video:video-read-buttons))
      ;(format t "Buttons ~X~%" *buttons*)
	  )))
