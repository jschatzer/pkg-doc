;;;; pkg-doc.lisp

; http://quickdocs.org/  <-------
; ev boobin to wrap lines/words in REAME ??  26.4.19

#|
; https://www.quicklisp.org/beta/releases.html

Project release   Provided systems
1am-20141106-git  1am
3b-swf-20120107-git 3b-swf-swc, 3b-swf

library - QL-RELEASE vs SYSTEM vs PKG  <----  4.4.19
;------------------------

Please note that some systems have different names than their projects. 
For example, to load cl-yacc's system, 
run (ql:quickload "yacc"), not (ql:quickload "cl-yacc").
|#

(in-package #:pkg-doc)
(named-readtables:in-readtable h:hh)

;--------------------------------------------------------
; TESTS, FOR TESTING ONLY    nach hinten
;--------------------------------------------------------
(defun random-pkg-with-nr-of-ext-symbols ()
  (let ((p (h:random-elt (current-packages)) ))
    (list p (length (pkg-symbols p)))))

#+quicklisp
(defun random-load-system ()
  (let ((s (h:random-elt (quicklisp-systems))))
    (ignore-errors (ql:quickload s))))

(defun random-pkg-info ()
  "test sys-info of current packages"
  (let ((p (h:random-elt (current-packages)) ))
    ;(format nil "Description:~&~a~2%Package-Name: ~a~%" (sys-info-clim *standard-output* p) p)))   ; def in gui-clim
    (format nil "Description:~&~a~2%Package-Name: ~a~%" (pkg-description *standard-output* p) p)))   ; def in gui-clim

#;(defun random-sys-info ()
  "test sys-info of quicklisp systems"
  (let ((s (h:random-elt (quicklisp-systems))))
    (ignore-errors (ql:quickload s))
    (format nil "Description:~&~a~2%Package-Name: ~a~%" (pkg-description *standard-output* (sys2pkg s)) s)))


;(packages-in-system :pkg-doc)
(defun packages-in-system (s)
  "new packages after loading a system, with dependency packages/systems"
  (h:ql (asdf:system-depends-on (asdf:find-system s)))
  (let ((x (list-all-packages)))
    ;(ql:quickload s)
    (ignore-errors (ql:quickload s))
    (mapcar (h:compose 'string-downcase 'package-name) (set-difference (list-all-packages) x))))

;28.4.2019
;(random-sys-info)
#;(defun random-sys-info ()
  "test sys-info of quicklisp systems"
  (let* ((s (h:random-elt (quicklisp-systems)))
         (dep (packages-in-system s)))
    (format nil "Description:~&~a~2%Package-Name: ~a~%" (pkg-description *standard-output* (sys2pkg s)) s)
    (format nil "system ~a has Packages: ~{~&- ~a~}" s dep)))


;--------------------------------
;; ev rename, or factor out dependency packages
; 26.4.2019
#;(defun packages-in-system (s)
  "new packages after loading a system, with dependency packages/systems"
  (let ((x (list-all-packages)))
    ;(ql:quickload s)
    (ignore-errors (ql:quickload s))
    (mapcar (h:compose 'string-downcase 'package-name) (set-difference (list-all-packages) x))))

; ("myweb.config" "com.ravenbrook.common-lisp-log" "trivial-utf-8" "myweb" "myweb.util" "myweb.handler")
;(packages-in-system :myweb)
;--------------------------------

(defun random-sys-info ()
  "test sys-info of quicklisp systems"
  (let* ((s (h:random-elt (quicklisp-systems)))
        (new-packages (packages-in-system s)))
    ;(ignore-errors (ql:quickload s))
    (format t "Description:~&~a~2%Package-Name: ~a~%" (pkg-description *standard-output* (sys2pkg s)) s)
    (format t "~&------------------------------------")
    (format t "~&loading system ~a loads these new packages: ~{~&- ~a~}" s new-packages)))




;The name "SLY" does not designate any package.
;The name "PARENSCRIPT-CLASSIC" does not designate any package.
;The name "MONKEYLIB-MARKUP-HTML" does not designate any package.
;The name "CL-JSON-TEMPLATE" does not designate any package.
;The name "CL-LIBYAML" does not designate any package.
;The name "ARCHITECTURE.BUILDER-PROTOCOL/TEST" does not designate any package.
;The name "EC2" does not designate any package.
;The name "FOREIGN-ARRAY" does not designate any package.

;(random-sys-info)


;--------------------------------------------------------
; 0) CONFIGURE
;--------------------------------------------------------
;1) adapt local-libs - optionally add a directory to quicklisp/local-projects??, ev append a list of dirs, ev config.lisp
;(defvar my-project-dir #P"~/src/lisp/") ; my-libs ??, export function?
(defvar my-project-dir #P"~/common-lisp/")
;--------------------------------------------------------
; 1) SYSTEM DESCRIPTION
;--------------------------------------------------------
;include pkg documentation string<-------
(define-symbol-macro sys-info
  `((s ,s)
    (pkg ,pkg)
    (nr (length (pkg-symbols pkg)))
    (a1 (car (asdf-description pkg)))
    (a2 (cadr (asdf-description pkg)))
    (a3 (readme-text pkg))
    (nick (package-nicknames pkg))))

;-----------------------
;mit match (a b ...)
#;(defun asdf-description (sys)
  (let ((x (asdf/system:find-system (pkg2sys sys))))
    (list (asdf/system:system-description x) (asdf/system:system-long-description x))))


;asdf/system:system-homepage   <--
(defun asdf-description (sys)
  (ignore-errors
    (let ((x (asdf/system:find-system (pkg2sys sys))))
      (list (asdf/system:system-description x) (asdf/system:system-long-description x)))))
;-----------------------


#| 
;EXAMPLES OF OTHER DOC FILES:   - 1) show with pdf-viewer, 2) display pdf in clim, 3) pdf2txt ??
sequence-iterators-20130813-darcs/doc/sequence-iterators.html
iterate-20180228-git/doc/tex/iterate-manual.pdf
|#
(defmacro readme-file (sys)
  "Look for a system's documentation file"
  `(or ,@(loop for x in '("README" "README.txt" "README.md" "README.markdown" "README.org" 
                          "doc/README" "doc/index.html" "docs/index.html")
               collect `(probe-file (asdf:system-relative-pathname (pkg2sys ,sys) ,x)))))

#|
;to test
(defun readme-file (sys)
  (loop for x in '("README" "README.txt" "README.md" "README.markdown" "README.org" 
                   "doc/README" "doc/index.html" "docs/index.html")
        while x do (ignore-errors (probe-file (asdf:system-relative-pathname (pkg2sys sys) x)))))
|#

(defun readme-text (p)
  "Get text from the system's docfile. If doc is html strip the tags"
  (let ((sys (pkg2sys p)))
    (or (ignore-errors
          (pre:match (file-namestring (readme-file sys))
            (#~m'html$' (strip-html (alexandria:read-file-into-string (readme-file sys))))
            (t (alexandria:read-file-into-string (readme-file sys)))))
        "No System Info?")))


;The name "CL-COLORS2" does not designate any package
(let ((sys-pkg '(("mcclim" . "CLIM")
                 ("alexandria" . "ALEXANDRIA.0.DEV")
                 ;("font-discovery" . "ORG.SHIRAKUMO.FONT-DISCOVERY") ; 14.10.2019,,, geht nicht
                 ;("oneliner" . "CL-ONELINER") ;??
                 ("cl-jpeg" . "JPEG"))))
  (defun pkg2sys (p) (or (car (rassoc p sys-pkg :test 'equal)) (string-downcase p)))
  (defun sys2pkg (p) (or (cdr (assoc p sys-pkg :test 'equal)) (string-upcase p))))

(defun strip-html (s) (#~s'<.*?>''gs s))

;--------------------------------------------------------
; 2) SYMBOL-TREE
;--------------------------------------------------------
; ev post-edit pkg-tree with css-selectors??
;  ev vereinfachen und function etc schon hier entfernen?
;  ("CL-FAD"
;   ("function:-" "function:-canonical-pathname"
;    ("function:-copy-" "function:-copy-file" "function:-copy-stream")
;    "function:-delete-directory-and-files"
(defun pkg-tree (p) 
  "exported symbols of a package in a hierachical tree-form"
  (cons (package-name p) 
        (insert-what (symbol-groups p))))

;; alfabet sort
;pkg-tree-a
;(defun pkg-tree (p) (cons (package-name p) (alfabet p)))

;; cl update-instance-  is empty bag
;  ("CL-FAD" "*default-template*" "cannot-create-temporary-file"
;    "canonical-pathname" ("copy-" "copy-file" "copy-stream")
;     "delete-directory-and-files"
(defun alfabet (p)
  (cons (package-name p) 
        (remove-empty-bags (pack (cw:sym2stg (sort (pkg-symbols p) 'string<))))))


;;; Hierarchy by symbolname ;;;

; (parts "a/b-c") ;("a/" "b-" "c" 
(defun parts (x) (#~d'(?<=[-./+])' x)) ; cl+ssl  -  +gray+ ??  <--- test

; (key 'a-b-c) -> "A-" ; (key 'a-b-c 1) -> "A-B-"
(defun key (s &optional (i 0))
 (#~s' ''g (stdutils:list-to-delimited-string (reverse (key% s i))))) 

;(key% 'a-b-c 1) -> (B- A-)
(defun key% (s i)
  "key ~ header"
  (cond ((zerop i) (list (nth i (parts s))))
        (t (cons (nth i (parts s)) (key% s (1- i))))))

(defun r-add-header (l ind) ; recursive-add-header list index
  (cons (key (car l) ind) (pack (reverse l) (1+ ind))))

;e.g. clim macro with- geht richtig
(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (r-add-header v i))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (r-add-header v i))
                 (pack (cdr l) i (list (car l)))))))

; e.g. pkg inner-conditional
;dieses "inner" macht with- probleme
; (... "inner" ("inner-" "inner-case" "inner-ccas ....
;(#~m'\W$' (car l))   hinzugetan
(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (atom (car l)) (consp (cadr l)) (#~m'\W$' (car l)) (#~m/(ppcre:quote-meta-chars (car l))/ (caadr l))) (cadr l))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;so geht clim macro with-  nicht richtig
(defun hierarchy-by-name (l)
  (remove-empty-bags (pack l)))

;--------------------------------------------
(defun pkg-symbols (pkg) (loop for s being the external-symbols of pkg collect s))

(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in (pkg-symbols pkg) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

(defun hierarchical-category (l) ;package category
  (hierarchy-by-name
    (cw:sym2stg l)))

;------------------------------------------
(in-package manifest)
;------------------------------------------
(manifest::define-category :SPECIAL-OPERATOR (symbol what)
  (:is (special-operator-p symbol)))

(manifest::define-category :CLIM-COLOR (symbol what)
  (:is (clim-color-p symbol)))

(define-category :constant (symbol what)
  (:is (constant-p symbol)))

(defun function-p (x)
  (and 
    (not (special-operator-p x))
    (ignore-errors (fdefinition x))))

(defun variable-p (name)
  (and (ignore-errors (boundp name))
       (not (#~m'^\+.+\+$' (symbol-name name)))))

;(constant-p 'clim:+fill+) ;t

(defun constant-p (x)
  (or (constantp x)
;      (and (#~m'^\+.+\+$' (symbol-name x))     ;includes +++ in cl
      (and (#~m'^\+[^+]+\+$' (symbol-name x)) 
           (not (manifest::clim-color-p x)))))

;#|
(defun clim-color-p (x)
  (and (member (#~s'\+''g (symbol-name x)) clim-internals::*xpm-x11-colors* :test 'equalp :key 'fourth)
       (#~m'^\+.+\+$' (symbol-name x))))
;|#

;------------------------------------------
;(in-package clim-pkg-doc)
(in-package pkg-doc)

;------------------------------------------

(defun symbol-groups (pkg)
  "group symbols into manifest::*categories*"
  (loop for what in (pre:match pkg 
                      (#~m'COMMON-LISP'  (cons :SPECIAL-OPERATOR manifest::*categories*))
                      (#~m'CLIM' (append manifest::*categories* '(:CLIM-COLOR)))
                      (t manifest::*categories*))
        for category = (sorted-symbols-in-a-category pkg what)
        when category collect (cons (#~s'$':' (string-downcase (princ-to-string what))) 
                                    (hierarchical-category category))))

(defun insert-what (l)
   (mapcar 'insert-what%% l))

;;;(insert-what% 'macro- 'abc-def-g)    <-- dzt error:
;(clim-pkg-doc::insert-what% 'macro- "abc-def-g") -> "MACRO-abc-def-g"
(defun insert-what% (w s) ;what symbol 
  (stdutils:list-to-delimited-string 
    (cons w (parts s)) ""))

(defun insert-what%% (l)
  (let ((w (#~s'$'-' (car l))))
    (cons w
      (labels ((rec (y)
                 (cond ((null y) nil)
                       ((atom y) (insert-what% w y))
                       (t (cons (rec (car y)) (rec (cdr y)))))))
        (rec (cdr l))))))

;--------------------------------------------------------
; ) MENU  create hierarchical menu to choose a package or a system
;--------------------------------------------------------
; 1) sorted lists of strings 
;---------------------------------------
;                     .. / systemname-.... /
;                        / cffi_0.19.0 /     und diverse andere
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
#;(defun ql-system-name (sys) 
  (#~s'(-|_)[^-_]+?(-git|-darcs|-svn|-http|-hg)?$'' 
   (second (#~d' / ' (princ-to-string sys))))) ; ev ql:system-name

#|
;; 6.4.2019 test new 
;#<QL-DIST:SYSTEM cl-pattern / cl-pattern-20140713-git / quicklisp 2019-03-07>
;#<QL-DIST:SYSTEM cl-pattern-benchmark / cl-pattern-20140713-git / quicklisp 2019-03-07>
;-----
;; über 1 seite, nicht jedes cl-glfw[*] ist ein pkg?? The name "CL-GLFW-OPENGL-ARB_HALF_FLOAT_VERTEX" does not designate any package.
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_float / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_gather / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_mirrored_repeat / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_multisample / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_rectangle / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_rg / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_rgb10_a2ui / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_texture_swizzle / cl-glfw-20150302-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cl-glfw-opengl-arb_timer_query / cl-glfw-20150302-git / quicklisp 2019-03-07>

 #<QL-DIST:SYSTEM cffi-examples / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-grovel / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-libffi / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-objects / cffi-objects-20140713-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-objects.tests / cffi-objects-20140713-git / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-tests / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-tests/example / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-toolchain / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi-uffi-compat / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi/c2ffi / cffi_0.20.0 / quicklisp 2019-03-07>
 #<QL-DIST:SYSTEM cffi/c2ffi-generator / cffi_0.20.0 / quicklisp 2019-03-07>

|#
; dzt 3188 systems, 6.4.19
(defun ql-system-name (sys) 
  (second (#~d' ' (princ-to-string sys))))

; "PDF" "PERLRE" "PKG-DOC" "PNGLOAD" "PROVE" "PROVE.ASDF" "PROVE.COLOR"
(defun current-packages ()  
  "loaded packages with external symbols"
  (sort 
    (remove-if-not 
      'pkg-symbols
      (mapcar 'package-name (list-all-packages)))
    'string<))

;----------------------------------
;                     -- not allways sys name !!
;#<SYSTEM cl-oneliner / oneliner-20131003-git / quicklisp 201
;(subst "cl-oneliner" "onliner"   .. müßte gehen
; pkg u sys = cl-oneliner

; ("abc" ...)
#+quicklisp
(defun quicklisp-systems () 
  (sublis '(
            ;;; ql-name . system-name
            ;("cl-mssql" . "mssql") ; 31.3.19 brauchts nicht mehr
            ;("cl-str" . "str")  ; 31.3.19
            ("cl-groupby" . "groupby")
            ("cl-sphinx" . "sphinx")
            )  ; (old . new)
  (sort 
    (remove-duplicates (mapcar 'ql-system-name (ql:system-list)) :test 'string=) 
    'string<)
  :test 'equal))

; ("abc" ...)
#+quicklisp
(defun local-systems ()
  (if (probe-file my-project-dir) (push my-project-dir ql:*local-project-directories*))
  (sort (ql:list-local-systems) 'string<))

#|
;doch überlegen, da viele nicht eigene in src/lisp
;;; ev gar nicht machen??
;; dzt nur afp-fts  backward-parenscript  cedilla  cldoc  clim-chess  ernestine  gsharp  Lindenmayer  mbe  mcclide  mcclim-desktop  rip-l-0.4  system-index.txt in local-projects
;geht nicht, da  es auch im gui dropdown menu getrennt werden muß!!! <-----
;separate dirs to see what is in what, 26.4.19
#+quicklisp
(defun local-systems ()
  (if (probe-file my-project-dir) 
    (append 
      ;simplyfy !!!
      ;dzt viele nicht directories --> nil ,eg permuted-symbol-index-a.html etc ??
      (sort
        (remove ".git"
          (remove nil
            (mapcar (lambda (x) (sixth (pathname-directory x)))
              (fad:list-directory
                my-project-dir))) :test 'equal) 'string<)
      (sort (ql:list-local-systems) 'string<))))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(defun pkg-description (s pkg)
  "system description"
  (let ((nr (length (pkg-symbols pkg)))
        (a1 (car (asdf-description pkg)))
        (a2 (cadr (asdf-description pkg)))
        (a3 (readme-text pkg)))
    (format s "Nickname: ~{~a ~}~%" (package-nicknames pkg))
    (with-drawing-options (s :ink +red+) (format s "~a " nr)) (format s "external-symbols~%")

    (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"-------------------------
 Package Documentaiton String
-------------------------~2%"))


    (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"-------------------------
 ASDF Description
-------------------------"))
  (with-drawing-options (s :text-face :bold) (format s "~&SHORT: ")) (format s "~a" a1)
  (with-drawing-options (s :text-face :bold) (format s "~2&LONG: ")) (format s "~a~%" a2)
  (with-drawing-options (s :text-face :bold :ink +red+) (format s 
"-------------------------
 README
-------------------------"))
  (format s "~&~a" a3)))


;rename sys-descr
(defun pkg-description (s pkg)
  "system description"
  (let ((nr (length (pkg-symbols pkg)))
        (a1 (car (asdf-description pkg)))
        (a2 (cadr (asdf-description pkg)))
        (a3 (readme-text pkg)))
    (format s "Nickname: ~{~a ~}~%" (package-nicknames pkg))
    (with-drawing-options (s :ink +red+) (format s "~a " nr)) (format s "external-symbols~%")

    (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"-------------------------
 Package Documentaiton String
-------------------------~2%"))


    (with-drawing-options (s :ink +red+ :text-face :bold) (format s 
"-------------------------
 ASDF Description
-------------------------"))
  (with-drawing-options (s :text-face :bold) (format s "~&SHORT: ")) (format s "~a" a1)
  (with-drawing-options (s :text-face :bold) (format s "~2&LONG: ")) (format s "~a~%" a2)
  (with-drawing-options (s :text-face :bold :ink +red+) (format s 
"-------------------------
 README
-------------------------"))
  (format s "~&~a" a3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;sys-info

(defmacro x ()
  `(let ,sys-info
     1))

(h:mac (x))

;;;;;
#|
;geht richtig!!!, 30.4.17
(defun pack (l &optional (i 0) v)
  (cond ((null l) (if (= 1 (length v)) v (list (cons (key (car v) i) (pack (reverse v) (1+ i))))))
        ((null v) (pack (cdr l) i (list (car l))))
        ((equal (key (car v) i) (key (car l) i)) (pack (cdr l) i (push (car l) v)))
        (t (cons (if (= 1 (length v))
                   (car v)
                   (cons (key (car v) i) (pack (reverse v) (1+ i))))
                 (pack (cdr l) i (list (car l)))))))
|#

;; das hatte ich bisher
;stört clim macro with-     <-----!! 
#;(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (consp (car l)) (notany #'consp (car l))) (cons (car l) (remove-empty-bags (cdr l))))
    ((and (= 2 (length l)) (atom (car l)) (consp (cadr l))) (remove-empty-bags (cadr l)))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;; test
#;(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
;    ((and (consp (car l)) (notany #'consp (car l))) (cons (car l) (remove-empty-bags (cdr l))))

    ((and (consp (car l)) (notany #'consp (car l))) (if (cdr l) 
                                                      (cons (car l) (remove-empty-bags (cdr l)))
                                                      (car l)))


    ((and (= 2 (length l)) (atom (car l)) (consp (cadr l))) (remove-empty-bags (cadr l)))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

; ev work with this
;cl spec-op multiple = emptybag, sonst gut
#;(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;;; das scheint richtig zu gehen, aber 1) ppcre error?, 2) ql menus sind zu kurz
#;(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (atom (car l)) (consp (cadr l)) (#~m/(car l)/ (caadr l))) (cadr l))  ; CL-PPCRE:PPCRE-SYNTAX-ERROR -  Quantifier '*' not allowed. at position 0 in string "*application-frame*"
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;1) package-symobols gehen fast perfekt, ev recursive oder über 2 level, -  clim slot-accessor: command-menu-  is emty-bag
;2) menus get truncated !! <--
#;(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (atom (car l)) (consp (cadr l)) (#~m/(ppcre:quote-meta-chars (car l))/ (caadr l))) (cadr l))  ; CL-PPCRE:PPCRE-SYNTAX-ERROR -  Quantifier '*' not allowed. at position 0 in string "*application-frame*"
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

#|
;; 1.4.19 
;test 1.4.19, so geht usocket gut, find package with empty bags: alexandria hash- hash-table, hash is empty <------   
; md5 fill is emtpy
; common-lisp special-operator multiple- is emtpy
(defun hierarchy-by-name (l)
  (pack l))



;; old
;damit geht clim macro with-  richtig
(defun hierarchy-by-symbolname (l)
  (pack l))
|#


#|
(defun hierarchical-category (l) ;package category
  (hierarchy-by-symbolname
    (cw:sym2stg l)))


;;; simple hack: (cw:sym2stg '(a b nil t)) ; ("a" "b" NIL "t") 
; diese NIL stört constants in clim und cl, so fehlt nil in beiden, geleg zu richten
(defun hierarchical-category (l) ;package category
  (remove nil
  (hierarchy-by-name
    (cw:sym2stg l))))
|#


#|
;; clim colors +cyan+ are in clim variables, remove them <----

; manifest definitions ev edit ??
(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(defun variable-p (name)
    (ignore-errors (boundp name)))

(defun function-p (name)
    (ignore-errors (fdefinition name)))
|#

#|
;simplify clim colors, make them fast
;;(240 248 255 "alice blue") (240 248 255 "AliceBlue") 
;so ginge es ~richtig, ist aber sehr langsam
(defun clim-color-p (x)
  (and (member (#~s'\+''g (symbol-name x)) clim-internals::*xpm-x11-colors* :test 'equalp :key (lambda (x) (#~s' '-'g (fourth x))))
       (#~m'^\+.+\+$' (symbol-name x))))
|#


#|
(defun clim-color-names ()
  (mapcar (lambda (x) (string-upcase (#~s'(.*)'+\1+'(#~s' '-'g x))))
          (mapcar #'fourth clim-internals::*xpm-x11-colors*)))

(defun clim-color-p (x)
  (member (symbol-name x) (clim-color-names) :test 'equal))
|#



; 2.4.19 -------------------------------------------
;alfabet sort, e.g. to see all with- symbols in cl or clim
#;(defun alfabet (p)
  (pkg-doc::pack (cw:sym2stg (sort (pkg-doc::pkg-symbols p) 'string<))))


