;;;; test.lisp

;(in-package #:pkg-doc)
(defpackage test (:use cl prove pkg-doc))
(in-package test)
(named-readtables:in-readtable h:hh)




;(random-list-item (pkg-doc:current-packages))
