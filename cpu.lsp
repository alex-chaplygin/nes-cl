(defvar PC 0)
(defvar A 0)
(defvar X 0)
(defvar Y 0)
(defvar ST 0)
(defvar SP 0)
(defvar op-adr 0); адрес операнда
(defvar cross 0); было ли пересечение страницы
(defvar add-cycle 0);дополнительные циклы

(defmacro make (name num)
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
(make "brk" 4)
(make "over" 6)
(make "neg" 7)

(defun st-push (v)
  (progn
    (mem:wrt (+ #x100 SP) v)
    (decb SP)))

(defun st-pop ()
  (progn
    (incb SP)
    (mem:rd (+ #x100 SP))))

(defmacro incb (v)
  `(progn
     (incf ,v)
     (when (> ,v 255)
       (setf ,v 0))))

(defmacro decb (v)
  `(progn
     (decf ,v)
     (when (< ,v 0)
       (setf ,v 255))))

(defun fetch ()
  (let ((v (mem:rd PC)))
    (incf PC)
    v))

(defun make-word (l h)
  (+ l (ash h 8)))

(defun fetch-word ()
  (make-word (fetch) (fetch)))

(defun page (adr) (ash adr -8))

(defun is-cross (adr ofs)
  (if (= (page adr) (page (+ adr ofs))) 0 1))

(defun acc () A)
(defun absolute ()
  (progn
    (setf op-adr (fetch-word))
    (mem:rd op-adr)))
(defun absx ()
  (progn
    (setf op-adr (fetch-word))
    (setf cross (is-cross op-adr X))
    (setf op-adr (+ X op))
    (mem:rd op-adr)))
(defun absy ()
  (progn
    (setf op-adr (fetch-word))
    (setf cross (is-cross op-adr Y))
    (setf op-adr (+ Y op))
    (mem:rd op-adr)))
(defun imm () (fetch))
(defun impl ())
(defun ind ()
  (progn
    (setf op-adr (fetch-word))
    (mem:rd (make-word (mem:rd op-adr) (mem:rd (+ 1 op-adr))))))
(defun xind ()
  (progn
    (setf op-adr (+ (fetch) X))
    (setf op-adr (logand op-adr #xFF))
    (setf op-adr (make-word (mem:rd op-adr (+ 1 op-adr))))
    (mem:rd op-adr)))
(defun indy ()
  (progn
    (setf op-adr (makeword (fetch) (fetch)))
    (setf op-adr (make-word (mem:rd op-adr (+ 1 op-adr))))
    (setf cross (is-cross op-adr Y))
    (setf op-adr (+ op-adr Y))
    (mem:rd op-adr)))
(defun rel ()
  (let ((a (fetch)))
    (if (< a 128) a (- a 256))))
(defun zero ()
  (progn
    (setf op-adr (logand (fetch) #xFF))
    (mem:rd op-adr)))
(defun zerox ()
  (progn
    (setf op-adr (+ X (logand (fetch) #xFF)))
    (mem:rd op-adr)))  
(defun zeroy ()
  (progn
    (setf op-adr (+ Y (logand (fetch) #xFF)))
    (mem:rd op-adr)))

(defmacro set-zero-neg (v)
  `(progn
     (when (= ,v 0) (|set-zero|))
     (when (< ,v 0)
       (progn
	 (|set-neg|)
	 (setf ,v (+ 256 ,v))))))

(defun sign (v)
  (ash (logand #xFF v) -7))

(defmacro set-carry (v)
  `(if (= (ash ,v -8) 1)
       (progn
	 (|set-carry|)
	 (setf ,v (- 256 ,v)))
       (|clear-carry|)))

(defun ADC (p)
  (let ((s (sign A)))
    (progn
      (setf A (+ A op (|get-carry|)))
      (set-zero-neg A)
      (set-carry A)
      (if (/= (sign A) s) (|set-over|) (|clear-over|))
      (setf add-cycle cross))))

(defstruct instr
  cmd mem cycle)

(defparameter *table* (make-array 256))

(defmacro op (c cmd mem cyc)
  `(setf (svref *table* ,c)
	 (make-instr :cmd ,cmd :mem ,mem :cycle ,cyc)))

(op #x69 #'ADC #'imm 2)
(op #x65 #'ADC #'zero 3)

(defun one-cmd ()
  (let* ((o (fetch)) (i (svref *table* o)))
    (progn
      (funcall (instr-cmd i) (funcall (instr-mem)))
      (instr-cycle i))))
