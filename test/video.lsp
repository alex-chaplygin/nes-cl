(defparameter *screen-width* 256)
(defparameter *screen-height* 240)
(defparameter *size* (* *screen-width* *screen-height*))

(defun setup-tiles ()
  (cart:read-ines "test/Tetris.nes")
  (ppu:write-chr0 (cart:get-chr 0))
  (mem:rd #x2002)
  (mem:wrt #x2006 0)
  (mem:wrt #x2006 0)
  (let ((ar #(#x41 #xc2 #x44 #x48 #x10 #x20 #x40 #x80 1 2 4 8 #x16 #x21 #x42 #x87)))
    (dotimes (i 16)
      (mem:wrt #x2007 0))))

(defun setup-palette ()
  (mem:rd #x2002)
  (mem:wrt #x2006 (ash ppu:+palette+ -8))
  (mem:wrt #x2006 (logand ppu:+palette+ #xFF))  
  (let ((pal #(#xF #x12 #x16 #x1A #xF #x15 #x18 #x1c #xF #x33 #x27 #x2c #xf #x35 #x38 #x3e #xF #x22 #x2B #x3A #xF #x15 #x18 #x1c #xF #x33 #x27 #x2c #xf #x35 #x38 #x3e)))
    (dotimes (i 32)
      (mem:wrt #x2007 (svref pal i)))))

(defun setup-names ()
  (mem:rd #x2002)
  (mem:wrt #x2006 (ash ppu:+name0+ -8))
  (mem:wrt #x2006 (logand ppu:+name0+ #xFF))  
  (dotimes (y 30)
    (dotimes (x 32)
					;      (mem:wrt #x2007 (mod (+ 1 x (* y 32)) 256))))
      (mem:wrt #x2007 63)))
  (dotimes (i 64)
    (mem:wrt #x2007 0))
  (dotimes (y 960)
    (mem:wrt #x2007 1))
  (dotimes (i 64)
    (mem:wrt #x2007 1))
  (dotimes (y 960)
    (mem:wrt #x2007 2))
  (dotimes (i 64)
    (mem:wrt #x2007 2))
  (dotimes (y 960)
    (mem:wrt #x2007 3))
  (dotimes (i 64)
    (mem:wrt #x2007 3)))

(defun setup-sprites ()
  (mem:wrt #x2003 0)
  (dotimes (i 512) (mem:wrt #x2004 #xFF))
  (mem:wrt #x2003 0)
  (dotimes (i 3)
    (dotimes (j 4)
    (mem:wrt #x2004 (* j 6))
    (mem:wrt #x2004 4)
    (mem:wrt #x2004 #xC0)
    (mem:wrt #x2004 (+ j (* i 20))))))

(defun setup-ppu ()
  (mem:wrt #x2000 #x80)
  (mem:wrt #x2001 #xFE))

(defun video-frame ()
  (cffi:with-foreign-object (buf :unsigned-char (* 256 240))
    (ppu:get-frame buf)
    (video:video-update buf)))

(defun main ()
  (video:video-init 2)
  ;(cart:read-ines "test/nestest.nes")
  (cart:read-ines "test/pacman.nes")
  (mem:write-bank1 (cart:get-prg 0))
  (mem:write-bank2 (cart:get-prg (- cart:*prg-count* 1)))
  (when (> cart:*chr-count* 0) (ppu:write-chr0 (cart:get-chr 0)))
  (setf q 1)
  (setf cycles 7)
  (cpu:interrupt :reset)
;  (setf cpu:PC #xC000)
  (loop while (= q 1)
	do (setf q (video:video-get-events))
	   (loop while (< cycles 29780) do 
	     (incf cycles (cpu:one-cmd)))
	   (ppu:vblanc-start)
	   (setf cycles 0)
	   (loop while (< cycles 2273) do 
	     (incf cycles (cpu:one-cmd)))
	   (ppu:vblanc-end)
	   (setf cycles 0)
	   (video-frame)
	   );(video-sleep 20))
  (video:video-close))

(defun main-loop ()
  (video:video-init 2)
  (setf q 1)
  (setf cycles 7)
  (loop while (= q 1) do
    (setf q (video:video-get-events))
    (video-frame))
  (video:video-close))

(setup-tiles)
(setup-palette)
(setup-names)
(setup-sprites)
(setup-ppu)
(main)
(main-loop)
(time (main2))
;(ppu:get-frame)
(video:video-close)
(close-lib)
