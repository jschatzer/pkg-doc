;;;; pkg-doc.asd

(asdf:defsystem #:pkg-doc
  :description "Describe pkg-doc here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on 
  (stdutils  
    clim-widgets nsort 
    ; webmacros ; hans-helper           
    manifest 
    alexandria
    repl-utilities)
  :components 
  ((:file "package")
   (:file "pkg-doc")
   (:file "gui-clim")
   ;(:file "gui-html")
   ;(:file "test")
   ))
