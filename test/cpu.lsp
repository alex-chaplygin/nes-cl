(defmacro make (name cod cyc check &body body)
  `(defun ,name (ac op res)
     (setf PC 0)
     (setf A ac)
     (mem:wrt PC ,cod)
     ,@body
     (assert (= (one-cmd) ,cyc))
     (assert (= ,check res))))

(make adc-imm #x69 2 A (mem:wrt (+ PC 1) op))
(make adc-zero #x65 3 A (mem:wrt (+ PC 1) #x10) (mem:wrt #x10 op))
(make adc-zerox #x75 4 A (setf X #xFF) (mem:wrt (+ PC 1) #x5)
      (mem:wrt #x4 op))
(make adc-abs #x6D 4 A (mem:wrt (+ PC 1) #x5) (mem:wrt (+ PC 2) #x2)
      (mem:wrt #x205 op))
(make adc-absx1 #x7D 4 A (mem:wrt (+ PC 1) #x5) (mem:wrt (+ PC 2) #x2)
      (setf X 3) (mem:wrt #x208 op))
(make adc-absx2 #x7D 5 A (mem:wrt (+ PC 1) #xff) (mem:wrt (+ PC 2) #x2)
      (setf X 2) (mem:wrt #x301 op))
(make adc-absy #x79 5 A (mem:wrt (+ PC 1) #xff) (mem:wrt (+ PC 2) #x2)
      (setf Y 2) (mem:wrt #x301 op))
(make adc-xind #x61 6 A (mem:wrt (+ PC 1) #x40) (mem:wrt #x42 #x2)
      (mem:wrt #x43 #x2) (setf X 2) (mem:wrt #x202 op))
(make adc-indy1 #x71 5 A (mem:wrt (+ PC 1) #x40) (mem:wrt #x40 #x2)
      (mem:wrt #x41 #x2) (setf Y 2) (mem:wrt #x204 op))
(make adc-indy2 #x71 6 A (mem:wrt (+ PC 1) #x40) (mem:wrt #x40 #x2)
      (mem:wrt #x41 #x2) (setf Y #xFF) (mem:wrt #x301 op))
(make asl-acc #xA 2 A)
(make asl-abs #xE 6 (mem:rd #x200) (mem:wrt (+ PC 1) 0)
      (mem:wrt (+ PC 2) 2) (mem:wrt #x200 op))
(make bcc-no #x90 2 PC (mem:wrt (+ PC 1) op))
(make bcc-yes #x90 3 PC (mem:wrt (+ PC 1) op))
(make bcc-cross #x90 4 PC (setf PC #x2FD) (mem:wrt PC #x90)
      (mem:wrt (+ PC 1) op))
(make bit-zero #x24 3 A (mem:wrt (+ PC 1) #x40) (mem:wrt #x40 op))
(make brk1 #x0 7 PC (mem:write-bank2 (make-array #x4000 :initial-element 3)))
(make cmp-imm #xC9 2 A (mem:wrt (+ PC 1) op))
(make jmp-ind #x6C 5 PC (mem:wrt (+ PC 1) (logand #xFF op))
      (mem:wrt (+ PC 2) (ash op -8)) (mem:wrt op #xAA) (mem:wrt (+ 1 op) #xBB))
(make jmp-abs #x4C 3 PC (mem:wrt (+ PC 1) (logand #xFF op))
      (mem:wrt (+ PC 2) (ash op -8)))
(make lsr-acc #x4A 2 A)
(make lsr-zero #x46 5 (mem:rd #x20) (mem:wrt (+ PC 1) #x20)
      (mem:wrt #x20 op))
(make rol-acc #x2A 2 A)

(defun adc-test (func)
  (|clear-carry|)
  (funcall func 0 10 10)
  (assert (= (|get-zero|) 0))
  (assert (= (|get-over|) 0))
  (funcall func 1 255 0)
  (assert (= (|get-zero|) 1))
  (funcall func 1 #xF0 #xF2)
  (assert (= (|get-neg|) 1))
  (assert (= (|get-carry|) 0))
  (funcall func 255 2 1)
  (assert (= (|get-carry|) 1))
  (funcall func 255 1 1)
  (assert (= (|get-over|) 1))
  (|clear-carry|)
  (funcall func 1 254 255)
  (funcall func 255 255 254)
  (|clear-carry|)
  (funcall func 0 0 0))

(defun asl-test (f)
  (funcall f 1 1 2)
  (assert (= (|get-carry|) 0))
  (funcall f 255 255 254)
  (assert (= (|get-carry|) 1)))

(defun bcc-test ()
  (|set-carry|)
  (bcc-no 0 10 2)
  (|clear-carry|)
  (bcc-yes 0 10 12)
  (bcc-yes 0 255 1)
  (bcc-cross 0 1 #x300))

(defun bit-test ()
  (bit-zero 1 1 1)
  (assert (= (|get-zero|) 0))
  (bit-zero 0 1 0)
  (assert (= (|get-zero|) 1))
  (bit-zero 0 #x8F 0)
  (assert (= (|get-neg|) 1))
  (assert (= (|get-over|) 0))
  (bit-zero 0 #x4F 0)
  (assert (= (|get-over|) 1)))

(defun brk-test ()
  (setf SP #xFF)
  (setf ST #x0)
  (brk1 0 0 #x303)
  (assert (= (|get-brk|) 1)))

(defun cmp-test ()
  (cmp-imm 10 10 10)
  (assert (= (|get-zero|) 1))
  (cmp-imm 10 1 10)
  (assert (= (|get-carry|) 1))
  (assert (= (|get-zero|) 0))
  (cmp-imm 0 1 0)
  (assert (= (|get-carry|) 0))
  (cmp-imm 10 20 10)
  (assert (= (|get-neg|) 1)))

(defun lsr-test ()
  (lsr-acc 3 0 1)
  (assert (= (|get-carry|) 1))
  (lsr-acc 0 0 0)
  (assert (= (|get-carry|) 0))
  (assert (= (|get-zero|) 1))
  (lsr-zero 0 3 1)
  (assert (= (|get-carry|) 1)))

(defun rol-test ()
  (|clear-carry|)
  (rol-acc 1 0 2)
  (assert (= (|get-carry|) 0))
  (|set-carry|)
  (rol-acc 1 0 3)
  (assert (= (|get-carry|) 0))
  (rol-acc #x80 0 0)
  (assert (= (|get-carry|) 1)))

(adc-test #'adc-imm)
(adc-test #'adc-zero)
(adc-test #'adc-zerox)
(adc-test #'adc-abs)
(adc-test #'adc-absx1)
(adc-test #'adc-absx2)
(adc-test #'adc-absy)
(adc-test #'adc-xind)
(adc-test #'adc-indy1)
(adc-test #'adc-indy2)
(asl-test #'asl-abs)
(asl-test #'asl-acc)
(bcc-test)
(bit-test)
(brk-test)
(cmp-test)
(jmp-ind 0 #x200 #xBBAA)
(jmp-abs 0 #x2FF #x2FF)
(lsr-test)
(rol-test)
