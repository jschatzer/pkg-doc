;;;; pkg-doc.asd

(asdf:defsystem #:pkg-doc
  :description "View package documentation in a clim-treeview"
  :author "<j.schatzer@tin.it>"
  :license  "BSD Simplified"
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
