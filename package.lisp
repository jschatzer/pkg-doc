;;;; package.lisp

(defpackage #:pkg-doc
  (:documentation "package and system info")
;  (:use #:cl)
  (:use clim clim-lisp)

  (:export pkg-tree create-menu
current-packages
quicklisp-systems
local-systems

;tests
pd
;random-list-item ; h:random-elt
random-pkg-with-nr-of-ext-symbols
random-load-system
    ))
