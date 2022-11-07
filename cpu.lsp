(defvar PC 0) ;указатель команд
(defvar A 0) ;аккумулятор
(defvar X 0) ;индекс 1
(defvar Y 0) ;индекс 2
(defvar ST 0) ;состояние
(defvar SP 0) ;указатель стека
(defvar op-adr 0); адрес операнда
(defvar cur-instr 0); текущая запись в таблице
(defvar cross 0); было ли пересечение страницы
(defvar add-cycle 0);дополнительные циклы

(defmacro make (name num)
  "Создание функций для флагов"
  `(progn
     (defun ,(intern (concatenate 'string "get-" name)) ()
       (ash (logand ST (ash 1 ,num)) (- 0 ,num)))
     (defun ,(intern (concatenate 'string "set-" name)) ()
       (setf ST (logior ST (ash 1 ,num))))
     (defun ,(intern (concatenate 'string "clear-" name)) ()
       (setf ST (logand ST (lognot (ash 1 ,num)))))))

(make "carry" 0)
(make "zero" 1)
(make "int" 2)
(make "dec" 3)
(make "brk" 4)
(make "over" 6)
(make "neg" 7)

(defmacro incb (v)
  "Увеличить переменную байт на 1"
  `(progn
     (incf ,v)
     (when (> ,v 255)
       (setf ,v 0))))

(defmacro decb (v)
  "Уменьшить переменную байт на 1"
  `(progn
     (decf ,v)
     (when (< ,v 0)
       (setf ,v 255))))

(defun st-push (v)
  "Запись в стек"
  (mem:wrt (+ #x100 SP) v)
  (decb SP))

(defun st-pop ()
  "Чтение из стека"
  (incb SP)
  (mem:rd (+ #x100 SP)))

(defun fetch ()
  "Загрузить очередной байт по указателю команд"
  (let ((v (mem:rd PC)))
    (incf PC)
    v))

(defun make-word (l h)
  "Склеить слово из 2 байт"
  (+ l (ash h 8)))

(defun fetch-word ()
  "Загрузить слово по указателю команд"
  (make-word (fetch) (fetch)))

(defun page (adr) (ash adr -8)) ;Вычислить номер страницы по адресу

(defun is-cross (adr ofs)
  "Возвращает 1, если пересечение страницы"
  (if (= (page adr) (page (+ adr ofs))) 0 1))

(defun to-signed (a)
  "Преобразовать к знаковому байту"
  (if (< a 128) a (- a 256)))

(defun acc () A) ;Режим адресации - аккумулятор

(defun absolute ()
  "Режим адресации - операнд из памяти, адрес - 2 байта"
  (setf op-adr (fetch-word))
  (mem:rd op-adr))

(defun absx ()
  "Режим адресации - операнд из памяти, адрес - 2 байта со смещением X"
  (setf op-adr (fetch-word))
  (setf cross (is-cross op-adr X))
  (setf op-adr (+ X op-adr))
  (mem:rd op-adr))

(defun absy ()
  "Режим адресации - операнд из памяти, адрес - 2 байта со смещением Y"
  (setf op-adr (fetch-word))
  (setf cross (is-cross op-adr Y))
  (setf op-adr (+ Y op-adr))
  (mem:rd op-adr))

(defun imm () (fetch)); Режим адресации - непосредственный операнд

(defun impl ()); Режим адресации - без операндов

(defun ind ()
  "Режим адресации - косвенный, адрес 2 байта содержит адрес операнда"
  (setf op-adr (fetch-word))
  (mem:rd (make-word (mem:rd op-adr) (mem:rd (+ 1 op-adr)))))

(defun xind ()
  "Режим адресации - адрес в нулевой странице со смещением X содержит адрес операнда"
  (setf op-adr (+ (fetch) X))
  (setf op-adr (logand op-adr #xFF))
  (setf op-adr (make-word (mem:rd op-adr) (mem:rd (+ 1 op-adr))))
  (mem:rd op-adr))

(defun indy ()
  "Режим адресации - адрес в нулевой странице содержит адрес, операнд находится со смещением Y"
  (setf op-adr (fetch))
  (setf op-adr (make-word (mem:rd op-adr) (mem:rd (+ 1 op-adr))))
  (setf cross (is-cross op-adr Y))
  (setf op-adr (+ op-adr Y))
  (mem:rd op-adr))

(defun rel ()
  "Режим адресации - операнд 1 байт со знаком"
  (to-signed (fetch)))

(defun zero ()
  "Режим адресации - адрес операнда в нулевой странице"
  (setf op-adr (logand (fetch) #xFF))
  (mem:rd op-adr))

(defun zerox ()
  "Режим адресации - адрес операнда в нулевой странице со смещением X"
  (setf op-adr (logand #xFF (+ X (fetch))))
  (mem:rd op-adr))

(defun zeroy ()
  "Режим адресации - адрес операнда в нулевой странице со смещением Y"
  (setf op-adr (+ Y (logand (fetch) #xFF)))
  (mem:rd op-adr))

(defmacro set-zero-neg (v)
  "Установить флаги нуля и знака"
  `(progn
     (if (= ,v 0) (|set-zero|) (|clear-zero|))
     (if (< (to-signed ,v) 0) (|set-neg|) (|clear-neg|))))

(defun sign (v)
  "Вычислить знак операнда"
  (ash (logand #xFF v) -7))

(defmacro set-carry (v)
  "Установить флаг переноса"
  `(if (> (ash ,v -8) 0)
       (progn
	 (|set-carry|)
	 (setf ,v (logand #xFF ,v)))
       (|clear-carry|)))

(defun no (adr op)
  "Пустой код операции"
  (error "NO OP"))

(defun ADC (adr op)
  "Сложение аккумулятора с операндом и переносом"
  (let ((s (sign A)))
    (setf A (+ A op (|get-carry|)))
    (set-carry A)
    (set-zero-neg A)
    (if (/= (sign A) s) (|set-over|) (|clear-over|))
    (setf add-cycle cross)))

(defun AND* (adr op)
  "Побитовое И аккумулятора с операндом"
  (setf A (logand A op))
  (set-zero-neg A)
  (setf add-cycle cross))

(defun ASL (adr op)
  "Арифметический сдвиг влево операнда"
  (let ((res (ash op 1)))
    (set-carry res)
    (set-zero-neg res)
    (if (eql (instr-mem cur-instr) #'acc)
	(setf A res) (mem:wrt adr res))))

(defmacro make-br (fun flag res)
  "Команды условного перехода"
  `(defun ,fun (adr op)
     (when (= (,flag) ,res)
       (setf add-cycle (+ 1 (is-cross PC op)))
       (setf PC (+ PC op)))))

(make-br BCC |get-carry| 0) ;Переход если нет переноса
(make-br BCS |get-carry| 1) ;Переход если перенос
(make-br BEQ |get-zero| 1) ;Переход если равно
(make-br BNE |get-zero| 0) ;Переход если не равно
(make-br BMI |get-neg| 1) ;Переход если меньше
(make-br BPL |get-neg| 0) ;Переход если больше
(make-br BVC |get-over| 0) ;Переход если не переполнение
(make-br BVS |get-over| 1) ;Переход если переполнение

(defun BIT* (adr op)
  "Тест битов"
  (set-zero-neg (logand A op))
  (if (= (ash op -7) 1) (|set-neg|) (|clear-neg|))
  (if (= (logand (ash op -6) 1) 1) (|set-over|) (|clear-over|)))

(defun BRK (adr op)
  "Программное прерывание IRQ"
  (|set-brk|)
  (interrupt mem:+irq-vector+))

(defun CLC (adr op) (|clear-carry|)) ;очистка переноса
(defun CLD (adr op) (|clear-dec|)) ;очистка десятичного режима
(defun CLI (adr op) (|clear-int|)) ;очистка запрета прерываний
(defun CLV (adr op) (|clear-over|)) ;очистка переполнения
(defun SEC (adr op) (|set-carry|)) ;установка переноса
(defun SED (adr op) (|set-dec|)) ;установка десятичного режима
(defun SEI (adr op) (|set-int|)) ;установка запрета прерываний

(defmacro make-comp (name reg)
  "Функция сравнения"
  `(defun ,name (adr op)
     (let ((res (- ,reg op)))
       (if (>= res 0) (|set-carry|) (|clear-carry|))
       (set-zero-neg res)
       (setf add-cycle cross))))

(make-comp CMP A) ;сравнение аккумулятора с операндом
(make-comp CPX X) ;сравнение X с операндом
(make-comp CPY Y) ;сравнение Y с операндом

(defmacro make-i (name f w &rest body)
  "Функция для увеличения/уменьшения"
  `(defun ,name (adr op)
     (let ((r ,w))
       (,f r)
       ,@body
       (set-zero-neg r))))

(make-i DEC decb op (mem:wrt adr r)) ;уменьшить ячейку памяти
(make-i DEX decb X (setf X r)) ;уменьшить X
(make-i DEY decb Y (setf Y r)) ;уменьшить Y
(make-i INC incb op (mem:wrt adr r)) ;увеличить ячейку памяти
(make-i INX incb X (setf X r)) ;увеличить X
(make-i INY incb Y (setf Y r)) ;увеличить Y

(defstruct instr ;Структура элемента таблицы инструкций
  cmd mem cycle) ;функция команды, функция адресации, число циклов

(defparameter *table* ;Таблица инструкций
  (make-array 256 :initial-element
	      (make-instr :cmd #'no :mem #'impl :cycle 0)))

(defmacro op (c cmd mem cyc)
  "Создать инструкцию"
  `(setf (svref *table* ,c)
	 (make-instr :cmd ,cmd :mem ,mem :cycle ,cyc)))

(op #x69 #'ADC #'imm 2)
(op #x65 #'ADC #'zero 3)
(op #x75 #'ADC #'zerox 4)
(op #x6D #'ADC #'absolute 4)
(op #x7D #'ADC #'absx 4)
(op #x79 #'ADC #'absy 4)
(op #x61 #'ADC #'xind 6)
(op #x71 #'ADC #'indy 5)
(op #x29 #'AND* #'imm 2)
(op #x25 #'AND* #'zero 3)
(op #x35 #'AND* #'zerox 4)
(op #x2D #'AND* #'absolute 4)
(op #x3D #'AND* #'absx 4)
(op #x39 #'AND* #'absy 4)
(op #x21 #'AND* #'xind 6)
(op #x31 #'AND* #'indy 5)
(op #x0A #'ASL #'acc 2)
(op #x06 #'ASL #'zero 5)
(op #x16 #'ASL #'zerox 6)
(op #x0E #'ASL #'absolute 6)
(op #x1E #'ASL #'absx 7)
(op #x90 #'BCC #'rel 2)
(op #xB0 #'BCS #'rel 2)
(op #xF0 #'BEQ #'rel 2)
(op #x30 #'BMI #'rel 2)
(op #xD0 #'BNE #'rel 2)
(op #x10 #'BPL #'rel 2)
(op #x24 #'BIT* #'zero 3)
(op #x2C #'BIT* #'absolute 4)
(op #x00 #'BRK #'impl 7)
(op #x18 #'CLC #'impl 2)
(op #xD8 #'CLD #'impl 2)
(op #x58 #'CLI #'impl 2)
(op #xB8 #'CLV #'impl 2)
(op #x38 #'SEC #'impl 2)
(op #xF8 #'SED #'impl 2)
(op #x78 #'SEI #'impl 2)
(op #xC9 #'CMP #'imm 2)
(op #xC5 #'CMP #'zero 3)
(op #xD5 #'CMP #'zerox 4)
(op #xCD #'CMP #'absolute 4)
(op #xDD #'CMP #'absx 4)
(op #xD9 #'CMP #'absy 4)
(op #xC1 #'CMP #'xind 6)
(op #xD1 #'CMP #'indy 5)
(op #xE0 #'CPX #'imm 2)
(op #xE4 #'CPX #'zero 3)
(op #xEC #'CPX #'absolute 4)
(op #xC0 #'CPY #'imm 2)
(op #xC4 #'CPY #'zero 3)
(op #xCC #'CPY #'absolute 4)
(op #xC6 #'DEC #'zero 5)
(op #xD6 #'DEC #'zerox 6)
(op #xCE #'DEC #'absolute 6)
(op #xDE #'DEC #'absx 7)
(op #xCA #'DEX #'impl 2)
(op #x88 #'DEY #'impl 2)
(op #xE6 #'INC #'zero 5)
(op #xF6 #'INC #'zerox 6)
(op #xEE #'INC #'absolute 6)
(op #xFE #'INC #'absx 7)
(op #xE8 #'INX #'impl 2)
(op #xC8 #'INY #'impl 2)

(defun one-cmd ()
  "Выполнить одну команду процессора, вернуть число циклов"
  (let* ((o (fetch)))
    (setf cur-instr (svref *table* o))
    (setf add-cycle 0)
    (setf cross 0)
    (let ((op (funcall (instr-mem cur-instr))))
      (funcall (instr-cmd cur-instr) op-adr op)
      (+ (instr-cycle cur-instr) add-cycle))))

(defun interrupt (vec)
  "Вызвать прерывание в процессоре"
  (st-push (logand PC #xFF))
  (st-push (ash PC -8))
  (st-push ST)
  (setf PC (make-word (mem:rd vec) (mem:rd (+ 1 vec)))))
