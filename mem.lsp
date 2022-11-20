(defpackage :mem
  (:use :cl)
  (:export :rd :wrt :write-bank1 :write-bank2 +nmi-vector+ +irq-vector+ +reset-vector+))
(in-package :mem)
(defparameter *table* ())
(defstruct rec
  upper
  read
  write)
(defparameter *memory* (make-array #x10000))

(defun mem (upper read write)
  "Запись в таблицу регионов памяти"
  (setf *table* (append *table* (list (make-rec :upper upper :read read :write write)))))

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
(mem #x2000 #'ram-read #'ram-write)
(mem #x4000 #'ppu:rd #'ppu:wrt)
(mem #x4014 #'io:rd #'io:wrt)
(mem #x4015 #'ppu:rd #'ppu:wrt)
(mem #x8000 #'rom-read #'cart-write)
(mem #x10000 #'rom-read #'map:wrt)

(defconstant +nmi-vector+ #xFFFA)
(defconstant +reset-vector+ #xFFFC)
(defconstant +irq-vector+ #xFFFE)
(defconstant +rom1+ #x8000)
(defconstant +rom2+ #xC000)

(defmacro m (name func par)
  `(defun ,name (,@par)
    (defun f (rec tail)
      (if (< adr (rec-upper rec))
	  (funcall (,func rec) ,@par)
	  (if (null tail)
	      (error ",name неверный адрес")
	      (f (car tail) (cdr tail)))))
    (f (car *table*) (cdr *table*))))
  
(m rd rec-read (adr))
(m wrt rec-write (adr val))

(defmacro w (name adr)
  `(defun ,name (bank)
     (do ((i 0 (+ i 1)))
	 ((= i (array-dimension bank 0)) 'done)
       (setf (svref *memory* (+ i ,adr)) (svref bank i)))))

(w write-bank1 +rom1+)
(w write-bank2 +rom2+)
