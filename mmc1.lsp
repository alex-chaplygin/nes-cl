(defpackage :cart
  (:use :cl)
  (:export :get-prg :get-chr :read-ines :*mirror* :+chr-size+ :*prg-count* :*chr-count*))

(defpackage :mmc1
  (:use :cl)
  (:export :wrt :prg-mode :chr-mode))
(in-package :mmc1)

(defvar reg #x10)
(defvar control 0)
(defvar prg-mode 0)
(defvar chr-mode 0)

(defun bit1 (d) (logand d 1))

(defun set5bit (bit)
  (setf reg (logior reg (ash bit 4))))
  
(defun reset-reg ()
  (progn
    (setf reg #x10)
    (mmc1-control (logior control #xC))))

(defun wrt (a d)
  (if (= #x80 (logand #x80 d)) (reset-reg)
      (progn (mmc1-control d)
      (if (= 1 (logand reg 1))
	  (progn
	    (setf reg (ash reg -1))
	    (switch a (set5bit (bit1 d))))
	  (progn
	    (setf reg (ash reg -1))
	    (set5bit (bit1 d)))))))

(defparameter *sw-table* ())
(defun sw (up func)
  (setf *sw-table* (append *sw-table* (list (cons up func)))))
  
(defun switch (a num)
  (defun f (lst)
    (if (null lst)
	(error "cannot switch mmc1")
	(let ((el (car lst)))
	  (if (< a (car el))
	      (funcall (cdr el) num)
	      (f (cdr lst))))))
  (f *sw-table*))

(defun mmc1-control (d)
  (setf control d)
  (let ((mir (logand d 3))
	(prg-m (logand (ash d -2) 3))
	(chr-m (logand (ash d -4) 1)))
    (setf prg-mode (cond ((< prg-m 2) 'switch-32)
			 ((= prg-m 2) 'fix-first)
			 ((= prg-m 3) 'fix-last)))
    (setf chr-mode (if (= chr-m 0) 'switch-8 'switch-4))
    (case prg-mode
      (fix-first (mem:write-bank1 (cart:get-prg 0)))
      (fix-last (mem:write-bank2 (cart:get-prg
				   (- cart:*prg-count* 1)))))
    (setf cart:*mirror* (cond ((< mir 2) :single)
			      ((= mir 2) :vertical)
			      ((= mir 3) :horizontal)))))
      

(defun mmc1-chr-bank0 (d)
  (let ((bank (if (eql chr-mode 'switch-8) (logand d #x1E)
		  (logand d #x1F))))
    (case chr-mode
      (switch-8 (ppu:write-chr0 (cart:get-chr bank)))
      (switch-4 (ppu:write-chr0 (subseq (cart:get-chr bank) 0 4096))))))

(defun mmc1-chr-bank1 (d)
  (when (eql chr-mode'switch-4)
    (ppu:write-chr1 (subseq (cart:get-chr d) 0 4096))))

(defun mmc1-prg-bank (d)
  (case prg-mode
    (switch-32 (let ((bank (logand d #xE)))
		  (mem:write-bank1 (cart:get-prg bank))
		  (mem:write-bank2 (cart:get-prg (+ 1 bank)))))
    (fix-first (mem:write-bank2 (cart:get-prg (logand d #xF))))
    (fix-last (mem:write-bank1 (cart:get-prg (logand d #xF))))))

(sw #xa000 #'(lambda (x) x))
(sw #xc000 #'mmc1-chr-bank0)
(sw #xe000 #'mmc1-chr-bank1)
(sw #x10000 #'mmc1-prg-bank)
