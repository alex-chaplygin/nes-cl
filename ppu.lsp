;(defpackage :ppu
;  (:use :cl)
;  (:export :rd :wrt :write-chr0 :write-chr1))

;(in-package :ppu)

(defconstant +chr0+ #x0000)
(defconstant +chr1+ #x1000)
(defconstant +name0+ #x2000)
(defconstant +palette+ #x3F00)

(defparameter *memory* (make-array #x4000)) ;память PPU
(defparameter *oam* (make-array 256)) ;память спрайтов
(defparameter *control* 0) ;регистр управления
(defparameter *mask* 0) ;регистр масок
(defparameter *status* 0) ;регистр статуса
(defparameter *byte-num* 0) ;номер байта для адресов
(defparameter *oam-adr* 0) ;адрес в памяти спрайтов
(defparameter *scroll* 0) ;регистр скроллинга
(defparameter *adr* 0) ;адрес в памяти PPU
(defparameter *read-buf* 0) ;промежуточный буфер для чтения

(defstruct sprite y tile atr x) ;структура спрайта

(defmacro mk (pre name num mask reg)
  "Создание функций для регистров"
  `(defun ,(intern (concatenate 'string (symbol-name pre)
				(symbol-name name))) ()
       (ash (logand ,reg (ash ,mask ,num)) (- 0 ,num))))
(defmacro mk/con (name num mask)
  `(mk control- ,name ,num ,mask *control*))
(defmacro mk/mas (name num mask)
  `(mk mask- ,name ,num ,mask *mask*))

(mk/con name 0 3) ;адрес таблицы имен
(mk/con inc 2 1) ;инкремент памяти 0 - 1, 1 - 32
(mk/con sprite-pat 3 1) ;номер таблицы шаблонов для спрайтов
(mk/con back-pat 4 1) ;номер таблицы шаблонов для фона
(mk/con sprite-size 5 1) ;размер спрайтов 8x8, 8x16
(mk/con gen-nmi 7 1) ;генерировать nmi при vblanc
(mk/mas grey 0 1) ;черно-белое изображение
(mk/mas show-back-8 1 1) ;показывать 8 левых пикселей фона
(mk/mas show-sprite-8 2 1) ;показывать спрайты в 8 левых пикселях
(mk/mas show-back 3 1) ;показывать фон
(mk/mas show-sprites 4 1) ;показывать спрайты
(mk/mas r 5 1) ;показывать красный цвет
(mk/mas g 6 1) ;показывать зеленый цвет
(mk/mas b 7 1) ;показывать синий цвет

(defun control (d) (setf *control* d)) ;записать регистр управления

(defun mask (d) (setf *mask* d)) ;записать регистр маски
(defun oam-adr (a) (setf *oam-adr* a)) ;записать адрес памяти спрайтов

(defun status ()
  "Прочитать регистр статуса"
  (let ((s *status*))
    (setf *byte-num* 1) ;сброс номера байта для адресов
    (setf *status* (logand s #x7F)) ;очистить флаг vblanc
    s))

(defun status-vblanc ()
  "Вернуть true, если состояние VBLANC"
  (if (= (logand *status* #x80) #x80) T nil))

(defun oam-data-rd ()
  "Прочитать данные спрайтов"
  (let ((d (svref *oam* *oam-adr*)))
    (when (not (status-vblanc)) (incf *oam-adr*))
    d))

(defmacro incb (v)
  "Увеличить переменную байт на 1"
  `(progn
     (incf ,v)
     (when (> ,v 255)
       (setf ,v 0))))

(defun oam-data-wrt (v)
  "Записать данные спрайтов"
  (setf (svref *oam* *oam-adr*) v)
  (incb *oam-adr*))

(defmacro make-adr (name reg)
  `(defun ,name (d)
     (if (= *byte-num* 0)
	 (setf ,reg (logior (logand ,reg #xFF00) d))
	 (setf ,reg (logior (logand ,reg #xFF) (ash d 8))))
     (setf *byte-num* (logand (+ 1 *byte-num*) 1))))

(make-adr scroll *scroll*) ;записать данные скроллинга X Y
(make-adr adr *adr*) ;записать адрес (старший байт, младший)

(defun inc-adr ()
  "Увеличить адрес"
  (setf *adr* (logand #x3FFF (+ *adr* (if (= (control-inc) 0) 1 32)))))

(defun data-rd ()
  "Чтение данных PPU"
  (let ((d (svref *memory* *adr*))
	(buf *read-buf*))
    (inc-adr)
    (if (< *adr* +palette+)
	(progn (setf *read-buf* d) buf)
	 d)))

(defun data-wrt (d)
  "Запись в PPU"
  (setf (svref *memory* *adr*) d)
  (inc-adr))

(defun oam-dma (page)
  "ДМА передача данных спрайтов"
  (dotimes (i 256)
    (oam-data-wrt))
  (setf cpu:add-cycle 514))

(defparameter *regs* (make-hash-table)) ;хеш-таблица регистров

(defstruct reg rd wrt) ;структура регистра
(defmacro reg (c r w)
  "Создать регистр"
  `(setf (gethash ,c *regs*) (make-reg :rd ,r :wrt ,w)))

(reg #x2000 nil #'control)
(reg #x2001 nil #'mask)
(reg #x2002 #'status nil)
(reg #x2003 nil #'oam-adr)
(reg #x2004 #'oam-data-rd #'oam-data-wrt)
(reg #x2005 nil #'scroll)
(reg #x2006 nil #'adr)
(reg #x2007 #'data-rd #'data-wrt)
(reg #x4014 nil #'oam-dma)

(defmacro rd/wr (name args f)
  "Чтение/запись регистров PPU"
  `(defun ,name (a ,@args)
     (let ((r (gethash a *regs*)))
       (when (null r) (error "PPU invalid address"))
       (when (null (,f r)) (error "PPU cannot rd/wr from reg"))
       (funcall (,f r) ,@args))))

(rd/wr rd () reg-rd)
(rd/wr wrt (v) reg-wrt)

(defmacro w (name adr)
  "Запись шаблонов"
  `(defun ,name (bank)
     (do ((i 0 (+ i 1)))
	 ((= i (array-dimension bank 0)) 'done)
       (setf (svref *memory* (+ i ,adr)) (svref bank i)))))

(w write-chr0 +chr0+) ;записать таблицу шаблонов 0
(w write-chr1 +chr1+) ;записать таблицу шаблонов 1

(defun vblanc-start ()
  "Начало кадровой развертки"
  (setf *status* (logior *status* #x80)) ;установить бит vblanc
  (when (= (control-gen-nmi) 1) (cpu:interrupt :nmi))) ;вызвать немаскируемое прерывание

(defun vblanc-end () (setf *status* 0)) ;конец кадровой развертки
