(map:set-mapper 1)
(map:wrt #xE000 #x80)
(map:wrt #xE000 1)
(map:wrt #xE000 1)
(map:wrt #xE000 1)
(map:wrt #xE000 1)
(assert (equal (map:wrt #xE000 1) '(31 :prg)))
(map:wrt #xE000 #x80)
(map:wrt #xE000 1)
(map:wrt #xE000 0)
(map:wrt #xE000 0)
(map:wrt #xE000 0)
(assert (equal (map:wrt #xE000 0) '(1 :prg)))
(map:wrt #xE000 #x80)
(map:wrt #x9fff 1)
(map:wrt #x9fff 0)
(map:wrt #x9fff 1)
(map:wrt #x9fff 0)
(assert (equal (map:wrt #x9fff 0) '(#x5 :control)))
(map:wrt #xA000 #x80)
(map:wrt #xA000 0)
(map:wrt #xA000 0)
(map:wrt #xA000 0)
(map:wrt #xA000 0)
(assert (equal (map:wrt #xA000 0) '(0 :chr0)))
(map:wrt #xDFFF #x80)
(map:wrt #xDFFF 0)
(map:wrt #xDFFF 0)
(map:wrt #xDFFF 0)
(map:wrt #xDFFF 0)
(assert (equal (map:wrt #xDFFF 1) '(#x10 :chr1)))
