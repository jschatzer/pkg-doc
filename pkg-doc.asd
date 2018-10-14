;;;; pkg-doc.asd

(asdf:defsystem #:pkg-doc
  :description "Describe pkg-doc here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hans-helper
               clim-widgets  ;                manifest nsort ;
               webmacros

               alexandria
               ;asdf
               stdutils
               repl-utilities)
  :components ((:file "package")
               (:file "pkg-doc")
               (:file "gui-clim")
               (:file "gui-html")
               (:file "test")))
