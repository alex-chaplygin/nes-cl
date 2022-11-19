(defpackage :mem
  (:use :cl)
  (:export :rd :wrt :write-bank1 :write-bank2 +nmi-vector+ +irq-vector+ +reset-vector+))
(defpackage :cpu
  (:use :cl)
  (:export :one-cmd :interrupt :add-cycle))
(defpackage :ppu
  (:use :cl)
  (:export :rd :wrt :write-chr0 :write-chr1 :+name0+ :+palette+ :get-frame :setup-tiles :*memory* :get-pattern :*adr* :get-tile :*scroll* :*oam*))
(defpackage :video
  (:use :cl)
  (:export :set-palette-mask))
(defpackage :cart
  (:use :cl)
  (:export :get-prg :get-chr :read-ines :*mirror*))

(in-package :ppu)

(defconstant +chr0+ #x0000) ;адрес таблицы шаблонов 0
(defconstant +chr1+ #x1000) ;адрес таблицы шаблонов 1
(defconstant +name0+ #x2000) ;адрес таблицы имен 0
(defconstant +name1+ #x2400) ;адрес таблицы имен 1
(defconstant +name2+ #x2800) ;адрес таблицы имен 2
(defconstant +name3+ #x2C00) ;адрес таблицы имен 3
(defconstant +attrib+ #x23C0) ;адрес атрибут
(defconstant +palette+ #x3F00) ;адрес палитры
(defconstant +width+ 256) ;ширина кадра
(defconstant +height+ 240) ;высота кадра
(defconstant +width-tiles+ 32) ;ширина кадра в тайлах
(defconstant +height-tiles+ 30) ;высота кадра в тайлах

(defparameter *memory* (make-array #x4000)) ;память PPU
(defparameter *oam* (make-array 256)) ;память спрайтов
(defparameter *control* 0) ;регистр управления
(defparameter *mask* 0) ;регистр масок
(defparameter *status* 0) ;регистр статуса
(defparameter *byte-num* 1) ;номер байта для адресов
(defparameter *oam-adr* 0) ;адрес в памяти спрайтов
(defparameter *scroll* 0) ;регистр скроллинга
(defparameter *adr* 0) ;адрес в памяти PPU
(defparameter *name-adr* 0) ;адрес текущего тайла в таблице имен
(defparameter *read-buf* 0) ;промежуточный буфер для чтения
(defparameter *frame* (make-array (* +width+ +height+))) ;буфер кадра
(defparameter *frame-pos* 0) ;позиция внутри кадра
(defparameter *fine-x* 0) ;смещение по x внутри тайла
(defparameter *fine-y* 0) ;смещение по y внутри тайла
(defparameter *screen-x* 0) ;x текущей точки экрана
(defparameter *screen-y* 0) ;y текущей точки экрана
(defparameter *begin-line* 0) ;адрес в таблице имен в начале строки
(defparameter *sprites-list* nil) ;список спрайтов на строке
(defparameter *sprite-transp* nil) ;прозрачная ли точка спрайта
(defparameter *back-transp* nil) ;прозрачная ли точка фона

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

;(defun set-palette-mask (r g b) nil) ;переопределяется в библиотеке

(defun mask (d)
  "Записать регистр маски"
  (setf *mask* d))
;  (video:set-palette-mask (mask-r) (mask-g) (mask-b)))

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
  (let ((d (rd-mem *adr*))
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
    (oam-data-wrt (mem:rd (+ i (ash page 8)))))
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

(defmacro w (name adr size)
  "Запись шаблонов"
  `(defun ,name (bank)
     (do ((i 0 (+ i 1)))
	 ((= i ,size) 'done)
       (setf (svref *memory* (+ i ,adr)) (svref bank i)))))

(w write-chr0 +chr0+ cart:+chr-size+) ;записать таблицу шаблонов 0
(w write-chr1 +chr1+ (ash cart:+chr-size+ -1)) ;записать таблицу шаблонов 1

(defun vblanc-start ()
  "Начало кадровой развертки"
  (setf *status* (logior *status* #x80)) ;установить бит vblanc
  (when (= (control-gen-nmi) 1) cpu:interrupt :nmi)) ;прерывание nmi 

(defun vblanc-end () (setf *status* 0)) ;конец кадровой развертки

(defun get-bit (byte num)
  (logand 1 (ash byte (- num 7))))

(defun mirror-h (adr)
  "Горизонтальное зеркалирование экранов"
  (if (or (and (>= adr +name1+) (< adr +name2+)) (>= adr +name3+))
      (- adr #x400) adr))

(defun mirror-v (adr)
  "Вертикальное зеркалирование"
  (if (>= adr +name2+) (- adr #x800) adr)) 

(defun mirror-s (adr) (logand adr #xF0FF)) ;один экран

(defun rd-mem (adr)
  "Прочитать значение из памяти с учетом зеркалирования"
  (let* ((m cart:*mirror*)
	 (a (logand adr #x2FFF))
	 (ad (if (>= adr +palette+) adr
		 (cond
		   ((eql m :horizontal) (mirror-h a))
		   ((eql m :vertical) (mirror-v a))
		   ((eql m :single) (mirror-s a))
		   ((eql m :4screen) a)))))
    (svref *memory* (logand ad #x3FFF))))

(defun get-tile-x () (logand #x1F *name-adr*)) ;получить координату x текущего тайла
(defun get-tile-y ()
  (logand #x1F (ash *name-adr* -5))) ;получить координату y текущего тайла
(defun get-table () (ash *name-adr* -10)) ;получить номер текущего экрана
(defun scroll-x () (ash *scroll* -8)) ;вычислить позицию перемещения X
(defun scroll-y () (logand *scroll* #xFF)) ;вычислить позицию перемещения Y

(defun clear-tile-x ()
  "Перейти на начало строки тайлов"
  (setf *name-adr* (logand *name-adr* #xFFE0)))

(defun set-tile-y (y)
  "Установить строку тайлов"
  (setf *name-adr* (logand *name-adr* #xFC1F))
  (incf *name-adr* (ash y 5)))

(defun apply-grey (c)
  "Если черно-белый режим, то применить его к цвету"
  (if (= (mask-grey) 1) (logand c #x30) c))

(defun get-color (pal back pix)
  "Получить цвет для палитры, (фона/спрайтов) и пикселя"
  (apply-grey (if (and (= (mask-show-back) 0) (= (mask-show-sprites) 0))
		  (svref *memory* +palette+) ;постоянный цвет, когда все выключено
      (svref *memory* (+ +palette+ (ash back 2) (ash pal 2) pix)))))

(defun get-pixel (tile-adr x)
  "Вычислить значение пикселя из тайла"
  (let ((blow (svref *memory* tile-adr))
	(bhigh (svref *memory* (+ tile-adr 8))))
    (+ (get-bit blow x)
       (ash (get-bit bhigh x) 1))))

(defun get-attrib ()
  "Получить атрибут(номер палитры) текущего тайла"
  (let* ((adr (logior +attrib+ (ash (get-table) 10);адрес тайла
		      (ash (get-tile-y) -1) (ash (get-tile-x) -2)))
	 (atr (rd-mem adr)) ;значение атрибута для квадрата 4x4
	 (x (ash (logand (get-tile-x) 3) -1)) ;координаты квадрата 2x2
	 (y (ash (logand (get-tile-y) 3) -1))
	 (pos (+ x (ash y 1)))) ;позиция внутри атрибута
    (logand (ash atr (- 0 (ash pos 1))) 3)))

(defun get-pattern (tile table fine-y)
  "Получить адрес тайла из таблицы шаблонов"
  (+ fine-y (ash tile 4) (ash table 12)))

(defun switch-screen (mask)
  "Переключиться на экран по горизонтали или вертикали"
  (setf *name-adr* (logxor *name-adr* mask)))

(defun next-tile ()
  "Переход на следующий тайл"
  (if (= (get-tile-x) (- +width-tiles+ 1))
      (progn (switch-screen #x400) (clear-tile-x))
      (incf *name-adr*))
  (setf *fine-x* 0))

(defun back-pixel ()
  "Вычислить точку фона"
  (if (= (mask-show-back) 0) (get-color 0 0 0)
      (let* ((tile (rd-mem (logior +name0+ *name-adr*))) ;получаем номер тайла
	     (tile-adr (get-pattern tile (control-back-pat) *fine-y*)) ;прочитать адрес строки тайла
	     (pix (get-pixel tile-adr *fine-x*))) ;вычисляем пиксель тайла
	(setf *back-transp* (= pix 0))
	(get-color (get-attrib) 0 pix)))) ;вычисляем цвет по атрибуту

(defun render-pixel (pix)
  "Отрисовать точку"
  (setf (svref *frame* *frame-pos*) pix))

(defun next-pixel ()
  "Перейти к следующей точке"
  (incf *fine-x*)
  (when (= *fine-x* 8) (next-tile)) ;перемещаемся на следующий тайл
  (incf *frame-pos*))

(defun sprite-atrib (spr)
  "Получить атрибут (палитру) спрайта"
  (+ 4 (logand (sprite-atr spr) 3)))

(defun get-sprite-pixel (a spr)
  "Получить точку спрайта или nil"
  (if (not (null a)) a
      (let ((sp-x (sprite-x spr))) ;левый угол спрайта
	(if (or (> sp-x *screen-x*)
		(<= (+ sp-x 8) *screen-x*)) nil ;не пересекается
	    (let* ((tile-y (- *screen-y* (sprite-y spr)))
		   (tile-adr (get-pattern (sprite-tile spr) (control-sprite-pat) tile-y))
		   (pix (get-pixel tile-adr (- *screen-x* sp-x))))
	      (setf *sprite-transp* (= pix 0))
	      (get-color (sprite-atrib spr) 1 pix))))))

(defun sprite-pixel ()
  "Вычислить точку спрайта"
  (if (= (mask-show-sprites) 0) nil ;если выключены спрайты выходим
      (reduce #'get-sprite-pixel *sprites-list* :initial-value nil)))

(defun combine (bp sp)
  "Наложить точки фона и спрайта"
  (if (null sp) bp
      (if *sprite-transp* bp sp)))

(defun begin-frame ()
  "Начало кадра, вычисляем начальный адрес в таблице имен"
  (let* ((coarse-x (ash (scroll-x) -3)) ;смещение по X в тайлах
	 (coarse-y (ash (scroll-y) -3))) ;смещение по Y в тайлах
    (setf *fine-x* (logand (scroll-x) 7)) ;смещение по X внутри тайла
    (setf *fine-y* (logand (scroll-y) 7))
    (setf *name-adr* (+ coarse-x (ash coarse-y 5) (ash (control-name) 10))) ;заполняем адрес тайла
    (setf *begin-line* *name-adr*)
    (setf *frame-pos* 0)))

(defun sprite-hit (spr)
  "Проверка пересекает ли спрайт текущую строчку"
  (let ((sp-y (sprite-y spr)))  ;верхняя строчка спрайта
    (and (<= sp-y *screen-y*) (> (+ sp-y 8) *screen-y*))))

(defun make-sprite-list (pos num list)
  "Добавить спрайт для текущей строки"
  (cond ((= pos 64) list) ;все просмотрено
	((= num 9) (setf *status* (logior *status* 2)) (cdr list)) ;переполнение спрайтов
	(t (let* ((adr (ash pos 2))
		  (y (svref *oam* adr))
		  (tile (svref *oam* (+ 1 adr)))
		  (atr (svref *oam* (+ 2 adr)))
		  (x (svref *oam* (+ 3 adr)))
		  (s (make-sprite :y y :tile tile :atr atr :x x)))
	     (if (sprite-hit s)
		 (make-sprite-list (+ pos 1) (+ 1 num) (cons s list))
		 (make-sprite-list (+ pos 1) num list))))))
	     
(defun scanline ()
  "Заполнить строку кадра"
  (setf *sprites-list*
	(if (= (mask-show-sprites) 1)
	    (make-sprite-list 0 0 nil) nil)) ;создать список спрайтов на текущей строке
  (dotimes (i +width+)
    (setf *screen-x* i)
    (let* ((bp (back-pixel))
	   (sp (sprite-pixel))
	   (cp (combine bp sp)))
      (render-pixel cp)
      (next-pixel))))

(defun next-line ()
  "Перейти к следующей строке"
  (setf *fine-x* (logand (scroll-x) 7))
  (setf *name-adr* *begin-line*)
  (if (< *fine-y* 7) (incf *fine-y*)
      (progn
	(setf *fine-y* 0)
	(let ((y (get-tile-y)))
	  (cond
	    ((= y (- +height-tiles+ 1))
	     (switch-screen #x800) (set-tile-y 0))
	    ((= y (- +width-tiles+ 1)) (set-tile-y 0))
	    (t (set-tile-y (+ y 1)))))))
  (setf *begin-line* *name-adr*))

(defun get-frame ()
  "Получить кадр"
  (begin-frame)
  (dotimes (i +height+)
    (setf *screen-y* i)
    (scanline)
    (next-line))
  *frame*)
