;;;; package.lisp

(defpackage #:pkg-doc
  (:documentation "package and system info")
;  (:use #:cl)
  (:use clim clim-lisp
        webmacros    ; cl ??
        )

  (:export pkg-tree create-menu

current-packages
quicklisp-systems
local-systems

;tests
pd
;random-list-item ; h:random-elt
random-pkg-with-nr-of-ext-symbols
random-load-system

;;; html part
ws-start ws-stop
    ))


;  (:use cl webmacros)
;(:export ws-start ws-stop))





;pkg-doc / webmacros    f p s  symbol conflict

