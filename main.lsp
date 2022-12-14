(declaim (optimize (speed 3) (safety 0)))
(load "~/quicklisp/setup.lisp")
(ql:quickload :cffi)
(load "video.lsp")
(load "ppu.lsp")
(load "io.lsp")
(load "mmc1.lsp")
(load "map.lsp")
(load "mem.lsp")
(load "cpu.lsp")
(load "cart.lsp")

(defvar q 0)
(defvar cycles 0)

(defun main (file)
  (video:video-init 2)
  (ppu:start)
  (cart:read-ines file)
  (mem:write-bank1 (cart:get-prg 0))
  (mem:write-bank2 (cart:get-prg (- cart:*prg-count* 1)))
  (when (> cart:*chr-count* 0) (ppu:write-chr0 (cart:get-chr 0)))
  (setf q 1)
  (setf cycles 0)
  (cpu:interrupt :reset)
  (cffi:with-foreign-object (buf :unsigned-char (* 256 240))
    (ppu:set-frame buf)
    (loop while (= q 1)
	  do (setf q (video:video-get-events))
	     (loop while (< cycles 29780) do 
	       (incf cycles)
	       (cpu:cycle)
	       (ppu:cycle)
	       (ppu:cycle)
	       (ppu:cycle))
	   (setf cycles 0)
	   (video:video-update buf)))
  (video:video-close))
