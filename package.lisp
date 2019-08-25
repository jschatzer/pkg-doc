;;;; package.lisp

(defpackage pkg-doc
  (:documentation "package and system info")
;  (:use #:cl)
  (:use clim clim-lisp
;        webmacros
        )

  (:export pkg-tree create-menu

current-packages
quicklisp-systems
local-systems

;tests
;pkg-doc
pd

;--------------------------------------------------------
;;; do not export ;;;;;  for testing only, to keep API simple
;random-list-item ; h:random-elt
random-pkg-with-nr-of-ext-symbols
random-load-system
random-pkg-info
random-sys-info
;--------------------------------------------------------


;;; html part
;ws-start ws-stop  ; brauchts nicht, use w:..
    ))



#|
(defpackage gui-clim
  (:documentation "")
  (:use clim clim-lisp)
  (:export pd))

(defpackage gui-html
  (:documentation "")
  (:use cl webmacros)
  (:export))
|#




;  (:use cl webmacros)
;(:export ws-start ws-stop))





;pkg-doc / webmacros    f p s  symbol conflict

