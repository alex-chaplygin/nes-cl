(defpackage :cart
  (:use :cl)
  (:export :get-prg :get-chr :read-ines :*mirror*))

(in-package :cart)

(defconstant +ines-head+ #x4e45531a) ;заголовок INES
(defconstant +prg-size+ #x4000) ;размер банка программы
(defconstant +chr-size+ #x2000) ;размер банка изображений
(defparameter *prg-count* 0) ;число банков программ
(defparameter *chr-count* 0) ;число банков изображений
(defparameter *mirror* 0) ;вид зеркалирования
(defparameter *prg* 0) ;память программ
(defparameter *chr* 0) ;память изображений

(defmacro mk/get (name count arr bits size)
  "Функции получения банков по номеру"
  `(defun ,name (num)
     (if (< num ,count)
	 (let ((start (ash num ,bits)))
	   (subseq ,arr start (+ start ,size)))
	 nil)))

(mk/get get-prg *prg-count* *prg* 14 +prg-size+) ;получить PRG банк
(mk/get get-chr *chr-count* *chr* 13 +chr-size+) ;получить CHR банк

(defun read4 (in)
  "Прочитать 4 байта"
  (setf res 0)
  (dotimes (i 4)
    (setf res (ash res 8))
    (incf res (read-byte in)))
  res)

(defun make-mirror (b) ;вид зеркалирования
  (if (= b 0) :horizontal :vertical))

(defmacro mk/rd (name arr size)
  "Чтение банков"
  `(defun ,name (in)
     (dotimes (i ,size)
       (setf (svref ,arr i) (read-byte in)))))

(mk/rd read-prg *prg* (* *prg-count* +prg-size+)) ;чтение PRG
(mk/rd read-chr *chr* (* *chr-count* +chr-size+)) ;чтение CHR

(defun read-ines (file)
  "Прочитать INES файл"
  (with-open-file (in file :direction :input :element-type 'unsigned-byte)
    (let ((head (read4 in)))
      (when (/= head +ines-head+) (error "Not INES"))
      (setf *prg-count* (read-byte in))
      (setf *chr-count* (read-byte in))
      (let* ((b (read-byte in))
	     (tr (logand (ash b -2) 1))
	     (ig (logand (ash b -7) 1)))
	(setf *mirror* (make-mirror (logand b 1)))
	(when (= ig 1) (setf *mirror* :4screen))
	(when (= tr 1) ) ;прочитать и записать трейнер
	(setf *prg* (make-array (* *prg-count* +prg-size+)))
	(setf *chr* (make-array (* *chr-count* +chr-size+)))
	(read-prg in)
	(read-chr in)))))
