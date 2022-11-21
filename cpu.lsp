(defpackage :cpu
  (:use :cl)
  (:export :one-cmd :interrupt :add-cycle :PC :cur-instr :SP))
(in-package :cpu)
(defvar PC 0) ;указатель команд
(defvar A 0) ;аккумулятор
(defvar X 0) ;индекс 1
(defvar Y 0) ;индекс 2
(defvar ST #x24) ;состояние
(defvar SP 0) ;указатель стека
(defvar op-adr 0); адрес операнда
(defvar cur-instr 0); текущая запись в таблице
(defvar cross 0); было ли пересечение страницы
(defvar add-cycle 0);дополнительные циклы

(defstruct instr ;Структура элемента таблицы инструкций
  cmd mem cycle) ;функция команды, функция адресации, число циклов

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

(defun st-push-pc ()
  "Сохранение PC в стек"
  (st-push (ash PC -8))
  (st-push (logand PC #xFF)))

(defun make-word (l h)
  "Склеить слово из 2 байт"
  (+ l (ash h 8)))

(defun st-pop-pc ()
  "Восстановить указатель команд из стека"
  (let* ((l (st-pop)) (h (st-pop)))
    (setf PC (make-word l h))))

(declaim (inline fetch))
(defun fetch ()
  "Загрузить очередной байт по указателю команд"
  (let ((v (mem:rd PC)))
    (incf PC)
    v))

(defun read-word (op-adr)
  "Прочитать слово из памяти"
  (make-word (mem:rd op-adr) (mem:rd (+ 1 op-adr))))

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

(defmacro make-abs (name reg)
  `(defun ,name ()
     (setf op-adr (fetch-word))
     (setf cross (is-cross op-adr ,reg))
     (setf op-adr (+ ,reg op-adr))
     (mem:rd op-adr)))

(make-abs absx X) ;Режим адресации - операнд из памяти, адрес - 2 байта со смещением X
(make-abs absy Y) ;Режим адресации - операнд из памяти, адрес - 2 байта со смещением Y

(defun imm () (fetch)); Режим адресации - непосредственный операнд

(defun impl ()); Режим адресации - без операндов

(defun ind ()
  "Режим адресации - косвенный, адрес 2 байта содержит адрес операнда"
  (setf op-adr (read-word (fetch-word)))
  (mem:rd op-adr))

(defun xind ()
  "Режим адресации - адрес в нулевой странице со смещением X содержит адрес операнда"
  (setf op-adr (+ (fetch) X))
  (setf op-adr (logand op-adr #xFF))
  (setf op-adr (read-word op-adr))
  (mem:rd op-adr))

(defun indy ()
  "Режим адресации - адрес в нулевой странице содержит адрес, операнд находится со смещением Y"
  (setf op-adr (fetch))
  (setf op-adr (read-word op-adr))
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
  (setf op-adr (logand (+ Y (fetch)) #xFF))
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

(defmacro set-bit (var pos bit)
  "Установить бит"
  `(setf ,var (if (= ,bit 1) (logior (ash 1 ,pos) ,var)
	 (logand (lognot (ash 1 ,pos)) ,var))))

(defun no (adr op)
  "Пустой код операции"
  (error "NO OP"))

(defmacro make-sum (name f carry)
  "Функции сложения, вычитания"
  `(defun ,name (adr op)
     (let ((b A))
       (setf A (,f A op ,carry))
       (if (> (logand (logxor b A) (logxor op A) #x80) 0)
	   (|set-over|) (|clear-over|))
       (set-carry A)
       (set-zero-neg A)
       (setf add-cycle cross))))

(make-sum ADC + (|get-carry|)) ;Сложение аккумулятора с операндом и переносом"
(make-sum SBC - (- 1 (|get-carry|))) ;Сложение аккумулятора с операндом и переносом"

(defmacro make-log (name f)
  "Логические функции"
  `(defun ,name (adr op)
     (setf A (,f A op))
     (set-zero-neg A)
     (setf add-cycle cross)))

(make-log AND* logand) ;Побитовое И аккумулятора с операндом
(make-log EOR logxor) ;Исключающее ИЛИ аккумулятора с операндом
(make-log ORA logior) ;Побитовое ИЛИ аккумулятора с операндом

(defmacro make-sh (name sh &rest body)
  "Функции сдвигов"
  `(defun ,name (adr op)
     (let ((res (ash op ,sh)))
       ,@body
       (set-zero-neg res)
       (if (eql (instr-mem cur-instr) #'acc)
	   (setf A res) (mem:wrt adr res)))))

(make-sh ASL 1 (set-carry res)) ;Арифметический сдвиг влево операнда
(make-sh LSR -1 (if (= (logand op 1) 1)
		    (|set-carry|) (|clear-carry|))) ;Логический сдвиг вправо
(make-sh ROL 1 (set-bit res 0 (|get-carry|)) ;Циклический сдвиг влево
	 (set-carry res) (set-zero-neg res))
(make-sh ROR -1 (set-bit res 7 (|get-carry|)) ;Циклический сдвиг вправо
	 (if (= (logand op 1) 1) (|set-carry|) (|clear-carry|)))

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
  (interrupt :brk))

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

(defun JMP (adr op) (setf PC adr)) ;безусловный переход

(defun JSR (adr op) (decf PC) (st-push-pc) (setf PC adr)) ;вызов подпрограммы

(defmacro make-ld (name reg)
  "Функции загрузки"
  `(defun ,name (adr op)
     (setf ,reg op)
     (set-zero-neg ,reg)
     (setf add-cycle cross)))

(make-ld LDA A)	;загрузить аккумулятор
(make-ld LDX X) ;загрузить X
(make-ld LDY Y) ;загрузить X

(defun NOP (adr op)) ;Пустая команда

(defun PHA (adr op) (st-push A)) ;Сохранить аккумулятор в стек
(defun PHP (adr op) (|set-brk|) (st-push ST)) ; Сохранить флаги в стек
(defun PLA (adr op) (setf A (st-pop)) (set-zero-neg A)) ;Восстановить аккумулятор
(defun PLP (adr op) (setf ST (logior #x20 (st-pop)))) ;Восстановить флаги

(defun RTI (adr op) (PLP adr op) (st-pop-pc)) ;Возврат из прерывания
(defun RTS (adr op) (st-pop-pc) (incf PC)) ;Возврат из подпрограммы

(defun STA (adr op) (mem:wrt adr A)) ;Сохранение аккумулятора
(defun STX (adr op) (mem:wrt adr X)) ;Сохранение X
(defun STY (adr op) (mem:wrt adr Y)) ;Сохранение Y

(defun TAX (adr op) (setf X A) (set-zero-neg X)) ;A -> X
(defun TAY (adr op) (setf Y A) (set-zero-neg Y)) ;A -> Y
(defun TSX (adr op) (setf X SP) (set-zero-neg X)) ;SP -> X
(defun TXA (adr op) (setf A X) (set-zero-neg A)) ;X -> A
(defun TYA (adr op) (setf A Y) (set-zero-neg A)) ;Y -> A
(defun TXS (adr op) (setf SP X)) ;X -> SP

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
(op #xE9 #'SBC #'imm 2)
(op #xE5 #'SBC #'zero 3)
(op #xF5 #'SBC #'zerox 4)
(op #xED #'SBC #'absolute 4)
(op #xFD #'SBC #'absx 4)
(op #xF9 #'SBC #'absy 4)
(op #xE1 #'SBC #'xind 6)
(op #xF1 #'SBC #'indy 5)
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
(op #x70 #'BVS #'rel 2)
(op #x50 #'BVC #'rel 2)
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
(op #x49 #'EOR #'imm 2)
(op #x45 #'EOR #'zero 3)
(op #x55 #'EOR #'zerox 4)
(op #x4D #'EOR #'absolute 4)
(op #x5D #'EOR #'absx 4)
(op #x59 #'EOR #'absy 4)
(op #x41 #'EOR #'xind 6)
(op #x51 #'EOR #'indy 5)
(op #x09 #'ORA #'imm 2)
(op #x05 #'ORA #'zero 3)
(op #x15 #'ORA #'zerox 4)
(op #x0D #'ORA #'absolute 4)
(op #x1D #'ORA #'absx 4)
(op #x19 #'ORA #'absy 4)
(op #x01 #'ORA #'xind 6)
(op #x11 #'ORA #'indy 5)
(op #x4C #'JMP #'absolute 3)
(op #x6C #'JMP #'ind 5)
(op #x20 #'JSR #'absolute 6)
(op #xA9 #'LDA #'imm 2)
(op #xA5 #'LDA #'zero 3)
(op #xB5 #'LDA #'zerox 4)
(op #xAD #'LDA #'absolute 4)
(op #xBD #'LDA #'absx 4)
(op #xB9 #'LDA #'absy 4)
(op #xA1 #'LDA #'xind 6)
(op #xB1 #'LDA #'indy 5)
(op #xA2 #'LDX #'imm 2)
(op #xA6 #'LDX #'zero 3)
(op #xB6 #'LDX #'zeroy 4)
(op #xAE #'LDX #'absolute 4)
(op #xBE #'LDX #'absy 4)
(op #xA0 #'LDY #'imm 2)
(op #xA4 #'LDY #'zero 3)
(op #xB4 #'LDY #'zerox 4)
(op #xAC #'LDY #'absolute 4)
(op #xBC #'LDY #'absx 4)
(op #x4A #'LSR #'acc 2)
(op #x46 #'LSR #'zero 5)
(op #x56 #'LSR #'zerox 6)
(op #x4E #'LSR #'absolute 6)
(op #x5E #'LSR #'absx 7)
(op #xEA #'NOP #'impl 2)
(op #x48 #'PHA #'impl 3)
(op #x08 #'PHP #'impl 3)
(op #x68 #'PLA #'impl 4)
(op #x28 #'PLP #'impl 4)
(op #x2A #'ROL #'acc 2)
(op #x26 #'ROL #'zero 5)
(op #x36 #'ROL #'zerox 6)
(op #x2E #'ROL #'absolute 6)
(op #x3E #'ROL #'absx 7)
(op #x6A #'ROR #'acc 2)
(op #x66 #'ROR #'zero 5)
(op #x76 #'ROR #'zerox 6)
(op #x6E #'ROR #'absolute 6)
(op #x7E #'ROR #'absx 7)
(op #x40 #'RTI #'impl 6)
(op #x60 #'RTS #'impl 6)
(op #x85 #'STA #'zero 3)
(op #x95 #'STA #'zerox 4)
(op #x8D #'STA #'absolute 4)
(op #x9D #'STA #'absx 5)
(op #x99 #'STA #'absy 5)
(op #x81 #'STA #'xind 6)
(op #x91 #'STA #'indy 6)
(op #x86 #'STX #'zero 3)
(op #x96 #'STX #'zeroy 4)
(op #x8E #'STX #'absolute 4)
(op #x84 #'STY #'zero 3)
(op #x94 #'STY #'zerox 4)
(op #x8C #'STY #'absolute 4)
(op #xAA #'TAX #'impl 2)
(op #xA8 #'TAY #'impl 2)
(op #xBA #'TSX #'impl 2)
(op #x8A #'TXA #'impl 2)
(op #x9A #'TXS #'impl 2)
(op #x98 #'TYA #'impl 2)

(defun fun-name (f) (symbol-name (nth-value 2 (function-lambda-expression f))))

(defun one-cmd ()
  "Выполнить одну команду процессора, вернуть число циклов"
 ; (format t "~X:" PC)
  (setf a1 (mem:rd (+ PC 1)))
  (setf a2 (mem:rd (+ PC 2)))
  (let* ((o (fetch)))
    (setf cur-instr (svref *table* o))
    (setf add-cycle 0)
    (setf cross 0)
    (let ((op (funcall (instr-mem cur-instr))))
      (funcall (instr-cmd cur-instr) op-adr op)
;      (format T " ~S ~S ~X ~X adr=~X op=~X A=~X X=~X Y=~X ST=~X~%" (fun-name (instr-cmd cur-instr)) (fun-name (instr-mem cur-instr)) a1 a2 op-adr op A X Y ST)
      (+ (instr-cycle cur-instr) add-cycle))))

(setf (get :brk 'vec) mem:+irq-vector+)
(setf (get :irq 'vec) mem:+irq-vector+)
(setf (get :nmi 'vec) mem:+nmi-vector+)
(setf (get :reset 'vec) mem:+reset-vector+)

(defun interrupt (in)
  "Вызвать прерывание в процессоре"
  (if (and (eql in :irq) (= (|get-int|) 1)) nil
      (progn
	(st-push-pc)
	(st-push ST)
	(when (eql in :reset) (setf SP #xFF) (|set-int|))
	(setf PC (read-word (get in 'vec))))))
