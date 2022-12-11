(defpackage :mem
  (:use :cl)
  (:export :rd :wrt :write-bank1 :write-bank2 +nmi-vector+ +irq-vector+ +reset-vector+))
(defpackage :cpu
  (:use :cl)
  (:export :one-cmd :interrupt :add-cycle :cycle))
(defpackage :ppu
  (:use :cl)
  (:export :start :rd :wrt :write-chr0 :write-chr1 :+name0+ :+palette+ :get-frame :setup-tiles :*memory* :get-pattern :*adr* :get-tile :*scroll* :*oam* :vblanc-start :vblanc-end :*oam-adr* :mirror-adr :set-frame :cycle :set-sprite0-hit :clear-sprite0-hit :*mask* :*cur-vram-adr* :*temp-vram-adr* :begin-frame))
(defpackage :video
  (:use :cl)
  (:export :set-palette-mask :video-init :video-update :video-close :video-get-events :video-sleep :video-read-buttons))
(defpackage :cart
  (:use :cl)
  (:export :get-prg :get-chr :read-ines :*mirror* :+chr-size+ :*prg-count* :*chr-count*))

(in-package :ppu)
;(declaim (inline apply-grey rd-mem get-bit mirror-h mirror-v mirror-s get-tile-y get-tile-x mask-show-back mask-grey mask-show-back-8 mask-show-sprites mask-show-sprite-8 control-back-pat get-table get-attrib control-sprite-size render-pixel combine get-pattern get-attrib))
(defconstant +scanline-cycles+ 341) ;число циклов для одной линии
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
(defconstant +tile-size+ 8) ;ширина и высота тайла
(defconstant +max-sprites+ 64) ;маскимальное количество спрайтов в OAM
(defconstant +sprites-in-line+ 8) ;маскимальное количество спрайтов на строке

(defparameter *memory* (make-array #x4000 :element-type :unsigned-byte)) ;память PPU
(defparameter *oam* (make-array (* 4 +max-sprites+) :element-type :unsigned-byte)) ;память спрайтов
(defparameter *control* 0) ;регистр управления
(defparameter *mask* 0) ;регистр масок
(defparameter *status* 0) ;регистр статуса
(defparameter *first-write* 0) ;номер байта для адресов
(defparameter *oam-adr* 0) ;адрес в памяти спрайтов
(defparameter *scroll* 0) ;регистр скроллинга
(defparameter *cur-vram-adr* 0) ;текущий адрес в памяти PPU
(defparameter *temp-vram-adr* 0) ;временный адрес в памяти PPU
;(defparameter *name-adr* 0) ;адрес текущего тайла в таблице имен
(defparameter *read-buf* 0) ;промежуточный буфер для чтения
(defparameter *frame* nil) ;(make-array (* +width+ +height+) :element-type :unsigned-byte)) ;буфер кадра
(defparameter *frame-pos* 0) ;позиция внутри кадра
(defparameter *fine-x* 0) ;смещение по x внутри тайла(скроллинг)
(defparameter *cur-fine-x* 0) ;смещение по x внутри тайла
(defparameter *fine-y* 0) ;смещение по y внутри тайла
(defparameter *screen-x* 0) ;x текущей точки экрана
(defparameter *screen-y* 0) ;y текущей точки экрана (номер строки)
;(defparameter *begin-line* 0) ;адрес в таблице имен в начале строки
(defparameter *sprites-list* nil) ;список спрайтов на строке
(defparameter *sprite-transp* nil) ;прозрачная ли точка спрайта
(defparameter *back-transp* nil) ;прозрачная ли точка фона
(defparameter *sprite-priority* 0) ;0 - впереди фона, 1 - позади фона
(defparameter *sprite-num* 0) ;номер текущего спрайта
(defparameter *cycles* 0) ;счетчик циклов
(declaim (unsigned-byte *control* *mask* *status* *first-write* *read-buf* *fine-x* *fine-y* *screen-x* *sprite-priority* *sprite-num*) (integer *oam-adr* *scroll* *cur-vram-adr* *name-adr* *frame-pos* *begin-line*) (fixnum *screen-y*))

(defstruct sprite y tile atr x num) ;структура спрайта

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

(defmacro mk/status (name bit)
  "Функции установки и сброса для регистра статуса"
  `(progn
     (defun ,(intern (concatenate 'string (symbol-name 'set-)
				(symbol-name name))) ()
       (setf *status* (logior *status* (ash 1 ,bit))))
     (defun ,(intern (concatenate 'string (symbol-name 'clear-)
				(symbol-name name))) ()
       (setf *status* (logand *status* (lognot (ash 1 ,bit)))))))

(mk/status over 5) ;флаг переплонения спрайтов
(mk/status sprite0-hit 6) ;флаг пересечения спрайта 0
(mk/status vblank 7) ;флаг режима кадровой развертки

(defmacro set-bits (reg offset num bits)
  `(progn
     (let ((mask (ash (- (ash 1 ,num) 1) ,offset)))
       (setf ,reg (logand ,reg (lognot mask)))
       (setf ,reg (logior ,reg (ash ,bits ,offset))))))

(defun control (d)
  (declare (unsigned-byte d))  
  (setf *control* d) ;записать регистр управления
  (set-bits *temp-vram-adr* 10 2 (logand d 3)));установка таблицы имен во временный адрес

;(defun set-palette-mask (r g b) nil) ;переопределяется в библиотеке

(defun mask (d)
  "Записать регистр маски"
  (declare (unsigned-byte d))  
  (setf *mask* d))
;  (video:set-palette-mask (mask-r) (mask-g) (mask-b)))

(defun oam-adr (a)
  (declare (integer a))  
  (setf *oam-adr* a)) ;записать адрес памяти спрайтов

(defun status ()
  "Прочитать регистр статуса"
  (let ((s *status*))
    (setf *first-write* 0) ;сброс номера байта для адресов
    (clear-vblank) ;очистить флаг vblanc
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
;  (declare (unsigned-byte v))  
  `(progn
     (incf ,v)
     (when (> ,v 255)
       (setf ,v 0))))

(defun oam-data-wrt (v)
  "Записать данные спрайтов"
;  (declare (unsigned-byte v))  
  (setf (svref *oam* *oam-adr*) v)
  (incb *oam-adr*))

(defun scroll (d)
  "Запись регистра скроллинга"
  (if (= *first-write* 0)
      (let ((coarse-x (ash d -3))) ;смещение в тайлах по x
	(set-bits *temp-vram-adr* 0 5 coarse-x) ;устанавливаем
	(setf *fine-x* (logand d 7)))
      (let ((fine-y (logand d 3))
	    (coarse-y (ash d -3)))
	(set-bits *temp-vram-adr* 12 3 fine-y)
	(set-bits *temp-vram-adr* 5 5 coarse-y)))
  (setf *first-write* (logand (+ 1 *first-write*) 1)))

(defun adr (d)
  "Запись адреса"
  (if (= *first-write* 0)
      (let ((temp (logand d #x3F)))
	(set-bits *temp-vram-adr* 8 6 temp)
	(set-bits *temp-vram-adr* 14 1 0))
      (progn
	(set-bits *temp-vram-adr* 0 8 d)
	(setf *cur-vram-adr* *temp-vram-adr*)))
  (setf *first-write* (logand (+ 1 *first-write*) 1)))

(defun inc-adr ()
  "Увеличить адрес"
  (incf *cur-vram-adr* (if (= (control-inc) 0) 1 32)))

(defun data-rd ()
  "Чтение данных PPU"
  (let ((d (rd-mem *cur-vram-adr*))
	(buf *read-buf*))
    (inc-adr)
    (if (< *cur-vram-adr* +palette+)
	(progn (setf *read-buf* d) buf)
	 d)))

(defun data-wrt (d)
  "Запись в PPU"
  (setf (svref *memory* (mirror-adr *cur-vram-adr*)) d)
  (inc-adr))

(defun oam-dma (page)
  "ДМА передача данных спрайтов"
  (declare (unsigned-byte page))  
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
       (if (null (,f r)) (error "PPU cannot rd/wr from reg"))
       (funcall (,f r) ,@args))))

(rd/wr rd () reg-rd)
(rd/wr wrt (v) reg-wrt)

(defmacro w (name adr size)
  "Запись шаблонов"
  `(defun ,name (bank)
     (dotimes (i ,size)
       (setf (svref *memory* (+ i ,adr)) (svref bank i)))))

(w write-chr0 +chr0+ cart:+chr-size+) ;записать таблицу шаблонов 0
(w write-chr1 +chr1+ (ash cart:+chr-size+ -1)) ;записать таблицу шаблонов 1

(defun vblanc-start ()
  "Начало кадровой развертки"
  (set-vblank) ;установить бит vblanc
  (when (= (control-gen-nmi) 1) (cpu:interrupt :nmi))) ;прерывание nmi 

(defun vblanc-end ()
;  (format t "Clear sprite 0 hit~%")
  (clear-sprite0-hit)
  (clear-vblank)) ;конец кадровой развертки

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

(defun mirror-palette (adr)
  "Зеркалирование адресов палитры"
  (if (and (>= adr #x3F10) (= (logand adr 3) 0)) (- adr #x10) adr))

(defun mirror-adr (adr)
  (let* ((m cart:*mirror*)
	 (a (logand adr #x2FFF))
	 (ad (if (>= adr +palette+) (mirror-palette adr)
		 (cond
		   ((eql m :horizontal) (mirror-h a))
		   ((eql m :vertical) (mirror-v a))
		   ((eql m :single) (mirror-s a))
		   ((eql m :4screen) a)))))
    (logand ad #x3FFF)))

(defun rd-mem (adr)
  "Прочитать значение из памяти с учетом зеркалирования"
    (svref *memory* (mirror-adr adr)))

(defun get-tile-x () (logand #x1F *cur-vram-adr*)) ;получить координату x текущего тайла
(defun get-tile-y () (logand #x1F (ash *cur-vram-adr* -5))) ;получить координату y текущего тайла
(defun get-table () (logand (ash *cur-vram-adr* -10) 3)) ;получить номер текущего экрана
(defun get-fine-y () (logand (ash *cur-vram-adr* -12) 7))
(defun sprites-height () (if (= (control-sprite-size) 0) 8 16)) ;высота спрайтов

(defun clear-tile-x ()
  "Перейти на начало строки тайлов"
  (setf *cur-vram-adr* (logand *cur-vram-adr* #xFFE0)))

(defun set-tile-y (y)
  "Установить строку тайлов"
  (declare (integer y))
  (set-bits *cur-vram-adr* 5 5 y))

(defun apply-grey (c)
  "Если черно-белый режим, то применить его к цвету"
  (declare (integer c))
  (if (= (mask-grey) 1) (logand c #x30) c))

(defun get-color (pal pix)
  "Получить цвет для палитры, (фона/спрайтов) и пикселя"
  (declare (integer pal pix))
  (apply-grey (if (or (= pix 0) (and (= (mask-show-back) 0) (= (mask-show-sprites) 0)))
		  (svref *memory* +palette+) ;постоянный цвет, когда все выключено или цвет фона
      (svref *memory* (+ +palette+ (ash pal 2) pix)))))

(defun get-pixel (tile-adr x)
  "Вычислить значение пикселя из тайла"
  (declare (integer tile-adr x))
  (let ((blow (svref *memory* tile-adr))
	(bhigh (svref *memory* (+ tile-adr 8))))
    (+ (get-bit blow x)
       (ash (get-bit bhigh x) 1))))

(defun get-attrib ()
  "Получить атрибут(номер палитры) текущего тайла"
  (let* ((adr (logior +attrib+ (ash (get-table) 10);адрес тайла
		       (ash (ash (get-tile-y) -2) 3) (ash (get-tile-x) -2)))
	 (atr (rd-mem adr)) ;значение атрибута для квадрата 4x4
	 (x (ash (logand (get-tile-x) 3) -1)) ;координаты квадрата 2x2
	 (y (ash (logand (get-tile-y) 3) -1))
	 (pos (+ x (ash y 1)))) ;позиция внутри атрибута
    (logand (ash atr (- 0 (ash pos 1))) 3)))

(defun get-pattern (tile table fine-y)
  "Получить адрес тайла из таблицы шаблонов"
  (let ((yy (if (>= fine-y +tile-size+) (+ fine-y +tile-size+) fine-y)))
    (+ yy (ash tile 4) (ash table 12))))

(defun switch-screen (mask)
  "Переключиться на экран по горизонтали или вертикали"
  (setf *cur-vram-adr* (logxor *cur-vram-adr* mask)))

(defun next-tile ()
  "Переход на следующий тайл"
  (if (= (get-tile-x) (- +width-tiles+ 1))
      (progn (switch-screen #x400) (clear-tile-x))
      (incf *cur-vram-adr*))
  (setf *cur-fine-x* 0))

(defun back-pixel ()
  "Вычислить точку фона"
;  (format t "~%temp-vram-adr ~X ~b~%" *temp-vram-adr* *temp-vram-adr*)
;  (format t "backpixel cur-vram-adr ~X ~b " *cur-vram-adr* *cur-vram-adr*)
;  (format t "line ~d tile ~d ~d fine ~d ~d~%" *screen-y* (get-tile-x) (get-tile-y) *cur-fine-x* (get-fine-y))
  (if (= (mask-show-back) 0) (get-color 0 0)
      (let* ((tile (rd-mem (logior +name0+ (logand *cur-vram-adr* #xFFF)))) ;получаем номер тайла
	     (tile-adr (get-pattern tile (control-back-pat) (get-fine-y))) ;прочитать адрес строки тайла
	     (pix (get-pixel tile-adr *cur-fine-x*))) ;вычисляем пиксель тайла
	(setf *back-transp* (= pix 0)) ;вычисляем прозрачность точки
	(get-color (get-attrib) pix)))) ;вычисляем цвет по атрибуту

(defun render-pixel (pix)
  "Отрисовать точку"
  (setf (cffi:mem-aref *frame* :unsigned-char *frame-pos*) pix))

(defun next-pixel ()
  "Перейти к следующей точке"
  (incf *cur-fine-x*)
  (when (= *cur-fine-x* +tile-size+) (next-tile)) ;перемещаемся на следующий тайл
  (incf *frame-pos*))

(defun sprite-atrib (spr)
  "Получить атрибут (палитру) спрайта"
  (+ 4 (logand (sprite-atr spr) 3)))

(defun get-sprite-tile (spr)
  "Получить тайл спрайта"
  (let ((tile (sprite-tile spr)))
    (if (= (control-sprite-size) 0) tile (ash tile -1))))

(defun get-sprite-table (spr)
  "Получить номер таблицы спрайтов"
  (if (= (control-sprite-size) 0) (control-sprite-pat)
      (logand (sprite-tile spr) 1)))

(defmacro spr/bit (name bit)
  `(defun ,name (spr) (logand (ash (sprite-atr spr) ,bit) 1)))

(spr/bit get-sprite-pri -5) ;получить приоритет спрайта
(spr/bit sprite-flip-horiz -6) ;получить отражение спрайта по горизонтали
(spr/bit sprite-flip-vert -7) ;получить отражение спрайта по вертикали

(defun get-sprite-pixel (a spr)
  "Получить точку спрайта или nil"
  (if (and (not (null a)) (not *sprite-transp*)) a 
       ;эту точку уже занял спрайт с меньшим номером
      (let ((sp-x (sprite-x spr))) ;левый угол спрайта
	(if (or (> sp-x *screen-x*)
		(<= (+ sp-x +tile-size+) *screen-x*)) nil ;не пересекается
		(let* ((tile-y (if (= (sprite-flip-vert spr) 1) ;отражение по вертикали
			 (- (- (sprites-height) 1) (- *screen-y* (sprite-y spr)))
			 (- *screen-y* (sprite-y spr))))
		       (tile-adr (get-pattern
				  (get-sprite-tile spr)
				  (get-sprite-table spr) tile-y))
		       (tile-x (if (= (sprite-flip-horiz spr) 1) ;отражение по горизонтали
				   (- (- +tile-size+ 1) (- *screen-x* sp-x))
				   (- *screen-x* sp-x)))
		       (pix (get-pixel tile-adr tile-x)))
		  (setf *sprite-transp* (= pix 0))
		  (setf *sprite-priority* (get-sprite-pri spr))
		  (setf *sprite-num* (sprite-num spr))
		  (get-color (sprite-atrib spr) pix))))))

(defun sprite-pixel ()
  "Вычислить точку спрайта"
  (if (= (mask-show-sprites) 0) nil ;если выключены спрайты выходим
      (reduce #'get-sprite-pixel *sprites-list* :initial-value nil)))

(defun set-zero-hit ()
  "Вычислить пересечение спрайта 0 с фоном"
  (when (and (= *sprite-num* 0) (not *back-transp*)
	     (not *sprite-transp*) (= (mask-show-back) 1))
;    (format t "Set zero hit~%")
    (set-sprite0-hit)))

(defun combine (bp sp)
  "Наложить точки фона и спрайта"
  (if (null sp) bp
      (progn (set-zero-hit)
	     (if (= *sprite-priority* 0) 
		 (if *sprite-transp* bp sp) ;спрайт впереди фона
		 (if *back-transp* sp bp))))) ;спрайт позади фона

(defun begin-frame ()
  "Начало кадра, вычисляем начальный адрес в таблице имен"
  ;(let* ((coarse-y (logand (ash *temp-vram-adr* -5) #x1F)) ;смещение по Y в тайлах
;	 (fine-y (ash *temp-vram-adr* -11)))
;    (set-bits *cur-vram-adr* 5 5 coarse-y)
					;    (set-bits *cur-vram-adr* 11 4 fine-y)
  (setf *cur-vram-adr* *temp-vram-adr*)
  (setf *cur-fine-x* *fine-x*)
  (setf *fine-y* (get-fine-y))
;  (format t "begin-frame~%temp-vram-adr ~X ~b~%" *temp-vram-adr* *temp-vram-adr*)
 ; (format t "cur-vram-adr ~X ~b~%" *cur-vram-adr* *cur-vram-adr*)
    (setf *frame-pos* 0))

(defun sprite-hit (spr)
  "Проверка пересекает ли спрайт текущую строчку"
  (let ((sp-y (sprite-y spr)))  ;верхняя строчка спрайта
    (and (<= sp-y *screen-y*)
	 (> (+ sp-y (sprites-height)) *screen-y*))))

(defun make-sprite-list (pos num list)
  "Добавить спрайт для текущей строки"
  (cond ((= pos +max-sprites+) list) ;все просмотрено
	((= num (+ 1 +sprites-in-line+)) (set-over) (cdr list)) ;переполнение спрайтов
	(t (let* ((adr (ash pos 2))
		  (y (svref *oam* adr))
		  (tile (svref *oam* (+ 1 adr)))
		  (atr (svref *oam* (+ 2 adr)))
		  (x (svref *oam* (+ 3 adr)))
		  (s (make-sprite :y (+ 1 y) :tile tile :atr atr :x x :num pos)))
	     (if (sprite-hit s)
		 (make-sprite-list (+ pos 1) (+ 1 num) (append list (list s)))
		 (make-sprite-list (+ pos 1) num list))))))

(defmacro mk/clip (name f)
  "Показывать фон/спрайты в левой колонке"
  `(defun ,name ()
     (and (= (,f) 0) (< *screen-x* 8))))

(mk/clip clip-sprite-left mask-show-sprite-8)
(mk/clip clip-back-left mask-show-back-8)

(defun scanline ()
  "Заполнить строку кадра"
 ; (format t "scanline ~d cur-vram-adr ~X ~b~%" *screen-y* *cur-vram-adr* *cur-vram-adr*)
  (setf *sprites-list*
	(if (= (mask-show-sprites) 1)
	    (make-sprite-list 0 0 nil) nil)) ;создать список спрайтов на текущей строке
  ;(format t "scanline after sprites ~d cur-vram-adr ~X ~b~%" *screen-y* *cur-vram-adr* *cur-vram-adr*)
  (dotimes (i +width+)
    (setf *screen-x* i)
    ;(format t "call back cur-vram-adr ~X ~b~%" *cur-vram-adr* *cur-vram-adr*)
    (let* ((bp (if (clip-back-left) (get-color 0 0) (back-pixel)))
	   (sp (if (clip-sprite-left) nil (sprite-pixel)))
	   (cp (combine bp sp)))
      (render-pixel cp)
      (next-pixel))))

(defun next-line ()
  "Перейти к следующей строке"
  (setf *cur-fine-x* *fine-x*)
  (setf *fine-y* (get-fine-y))
  (if (< *fine-y* 7) (incf *fine-y*)
      (progn
	(setf *fine-y* 0)
	(let ((y (get-tile-y)))
	  (cond
	    ((= y (- +height-tiles+ 1))
	     (switch-screen #x800) (set-tile-y 0))
	    ((= y (- +width-tiles+ 1)) (set-tile-y 0))
	    (t (set-tile-y (+ y 1)))))))
  (set-bits *cur-vram-adr* 0 5 (logand *temp-vram-adr* #x1F))
  (set-bits *cur-vram-adr* 10 1 (logand (ash *temp-vram-adr* -10) 1))
  (set-bits *cur-vram-adr* 12 3 *fine-y*)
  ;(format t "line ~d cur-vram-adr ~X ~b~%" *screen-y* *cur-vram-adr* *cur-vram-adr*)
) ;вернулись в начало строки

(defun set-frame (frame)
  (setf *screen-y* 261)
  (setf *cycles* 0)
  (setf *frame* frame))

(defun scanline-cycle ()
  "Линия цикла PPU"
  (if (and (< *screen-y* +height+) (= (logand *screen-y* 1) 1))
      (setf *cycles* 1) (setf *cycles* 0))
;  (format t "PPU scanline cycle ~d~%" *screen-y*)
  (cond ((= *screen-y* 261) (vblanc-end) (begin-frame) (setf *screen-y* -1)) ;перед отрисовкой prerender
	((< *screen-y* +height+) (scanline) (next-line)) ;отрисовка линии
	((= *screen-y* 241) (vblanc-start))) ;начало VBlank
  (incf *screen-y*))

(defun cycle ()
  "Один цикл PPU"
;  (format t "PPU cycle ~d~%" *cycles*)
  (if (>= *cycles* +scanline-cycles+) (scanline-cycle)
      (incf *cycles*)))

(defun start ()
  (setf *control* 0)
  (setf *mask* 0)
  (setf *scroll* 0)
  (setf *oam-adr* 0)
  (setf *read-buf* 0)
  (setf *first-write* 0)
  (setf *status* 0))
