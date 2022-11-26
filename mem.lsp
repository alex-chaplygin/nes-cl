(defpackage :mem
  (:use :cl)
  (:export :rd :wrt :write-bank1 :write-bank2 +nmi-vector+ +irq-vector+ +reset-vector+))
(in-package :mem)
(declaim (inline ram-read ram-write rom-read rd wrt))
(defparameter *table* (make-array 7))
(defstruct rec
  upper
  read
  write)
(defparameter *memory* (make-array #x10000 :element-type :unsigned-byte))

(defun mem (i upper read write)
  "Запись в таблицу регионов памяти"
  (setf (svref *table* i) (make-rec :upper upper :read read :write write)))

(defun ram-read (adr)
  "Чтение ОЗУ"
  (svref *memory* (logand adr #x7ff)))

(defun ram-write (adr val)
  "Запись ОЗУ"
  (setf (svref *memory* (logand adr #x7ff)) val))

;; чтение ROM
(defun rom-read (adr) (svref *memory* adr))

;; запись в картридж
(defun cart-write (adr val) (setf (svref *memory* adr) val))

;; таблица памяти
(mem 0 #x2000 #'ram-read #'ram-write)
(mem 1 #x4000 #'ppu:rd #'ppu:wrt)
(mem 2 #x4014 #'io:rd #'io:wrt)
(mem 3 #x4015 #'ppu:rd #'ppu:wrt)
(mem 4 #x4018 #'io:rd #'io:wrt)
(mem 5 #x8000 #'rom-read #'cart-write)
(mem 6 #x10000 #'rom-read #'map:wrt)

(defconstant +nmi-vector+ #xFFFA)
(defconstant +reset-vector+ #xFFFC)
(defconstant +irq-vector+ #xFFFE)
(defconstant +rom1+ #x8000)
(defconstant +rom2+ #xC000)

(defmacro m (name func par)
  `(defun ,name (,@par)
     (let ((adr (logand adr #xFFFF)))
       (do ((i 0 (+ i 1)))
	   ((> i 7) 'done)
	 (let ((rec (svref *table* i)))
	   (if (< adr (rec-upper rec))
	       (return (funcall (,func rec) ,@par))))))))
  
(m rd rec-read (adr))
(m wrt rec-write (adr val))

(defmacro w (name adr)
  `(defun ,name (bank)
     (do ((i 0 (+ i 1)))
	 ((= i (array-dimension bank 0)) 'done)
       (setf (svref *memory* (+ i ,adr)) (svref bank i)))))

(w write-bank1 +rom1+)
(w write-bank2 +rom2+)
