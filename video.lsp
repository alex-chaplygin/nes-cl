;(defpackage :video
;  (:use :cl)
;  (:export :set-palette-mask :start))

;(in-package :video)

(defun init-lib ()
  (cffi:define-foreign-library video
    (t (:default "./video")))
  (cffi:use-foreign-library video)
  (cffi:defcfun "video_init" :void (scale :int))
  (cffi:defcfun "video_update" :void (buf :pointer))
  (cffi:defcfun "video_close" :void)
  (cffi:defcfun "video_get_events" :int)
  (cffi:defcfun "set_palette_mask" :void (r :int) (g :int) (b :int))
  (cffi:defcfun "video_sleep" :int (del :long)))

(defun close-lib ()
  (cffi:close-foreign-library 'video))

(init-lib)
