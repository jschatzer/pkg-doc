;;;; pkg-doc.lisp

(in-package #:pkg-doc)
(named-readtables:in-readtable h:hh)

;--------------------------------------------------------
; TESTS
;--------------------------------------------------------
#;(defun random-list-item (lst)
  (nth (random (1- (length lst))) lst))
;(random-list-item '(1 2 3 4 5))

#;(defun random-pkg-with-nr-of-ext-symbols ()
  (let ((p (random-list-item (current-packages)) ))
    (list p
          (length (pkg-symbols p)))))

#;(defun random-load-system ()
  (let ((s (random-list-item (pkg-doc:quicklisp-systems))))
    ;(list s
          (ignore-errors (ql:quickload s))))

#|
(defun random-load-system ()
  (let ((s (pkg-doc:random-list-item (pkg-doc:quicklisp-systems))))
    (list s
          (multiple-value-bind (a b) (ignore-errors (ql:quickload s))
           (list a b)))))
|#

(defun random-pkg-with-nr-of-ext-symbols ()
  (let ((p (h:random-elt (current-packages)) ))
    (list p (length (pkg-symbols p)))))

(defun random-load-system ()
  (let ((s (h:random-elt (quicklisp-systems))))
    (ignore-errors (ql:quickload s))))




;--------------------------------------------------------
; 0) CONFIGURE
;--------------------------------------------------------
;1) adapt local-libs - optionally add a directory to quicklisp/local-projects??, ev append a list of dirs, ev config.lisp
(defvar my-project-dir #P"~/src/lisp/") ; my-libs ??, export function?

;--------------------------------------------------------
; 1) SYSTEM DESCRIPTION
;--------------------------------------------------------
;include pkg documentation string<-------

(defun pkg-description (s pkg)
  "system description"
  (let ((nr (length (pkg-symbols pkg)))
        (a1 (car (asdf-description pkg)))
        (a2 (cadr (asdf-description pkg)))
        (a3 (readme-text pkg)))
    (format s "Nickname: ~{~a~}~%" (package-nicknames pkg))
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

;-----------------------
;mit match (a b ...)
(defun asdf-description (sys)
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
                 ;("oneliner" . "CL-ONELINER") ;??
                 ("cl-jpeg" . "JPEG"))))
  (defun pkg2sys (p) (or (car (rassoc p sys-pkg :test 'equal)) (string-downcase p)))
  (defun sys2pkg (p) (or (cdr (assoc p sys-pkg :test 'equal)) (string-upcase p))))

(defun strip-html (s) (#~s'<.*?>''gs s))

;--------------------------------------------------------
; 2) SYMBOL-TREE
;--------------------------------------------------------
; ev post-edit pkg-tree with css-selectors??
(defun pkg-tree (p) (cons (package-name p) (insert-what (symbol-groups p))))

;;; Hierarchy by symbolname ;;;

; (parts 'a-b-c) -> ("a-" "b-" "c")
#;(defun parts (x) (#~d'(?<=-)' x))

; (parts "a/b-c") ;("a/" "b-" "c" 
(defun parts (x) (#~d'(?<=[-./+])' x)) ; cl+ssl bisher das einzige, ev remove + 

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

;stört clim macro with-     <-----!! 
(defun remove-empty-bags (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((and (consp (car l)) (notany #'consp (car l))) (cons (car l) (remove-empty-bags (cdr l))))
    ((and (= 2 (length l)) (atom (car l)) (consp (cadr l))) (remove-empty-bags (cadr l)))
    (t (cons (remove-empty-bags (car l)) (remove-empty-bags (cdr l))))))

;;;; ( - / + . )  trivia.level0 asdf/abc cl+ssl
;so geht clim macro with-  nicht richtig
(defun hierarchy-by-symbolname (l)
  (remove-empty-bags (pack l)))

#|
;damit geht clim macro with-  richtig
(defun hierarchy-by-symbolname (l)
  (pack l))
|#

;--------------------------------------------
(defun pkg-symbols (pkg) (loop for s being the external-symbols of pkg collect s))

(defun sorted-symbols-in-a-category (pkg what)
  "return a sorted list of all symbols in a category"
  (sort (loop for sym in (pkg-symbols pkg) 
              when (manifest::is sym what) collect sym) #'nsort:nstring<))

#|
(defun hierarchical-category (l) ;package category
  (hierarchy-by-symbolname
    (cw:sym2stg l)))
|#

;;; simple hack: (cw:sym2stg '(a b nil t)) ; ("a" "b" NIL "t") 
; diese NIL stört constants in clim und cl, so fehlt nil in beiden, geleg zu richten
(defun hierarchical-category (l) ;package category
  (remove nil
  (hierarchy-by-symbolname
    (cw:sym2stg l))))

;------------------------------------------
(in-package manifest)
;------------------------------------------
(manifest::define-category :SPECIAL-OPERATOR (symbol what)
  (:is (special-operator-p symbol)))

(manifest::define-category :CLIM-COLOR (symbol what)
  (:is (clim-color-p symbol)))

(define-category :constant (symbol what)
  (:is (constant-p symbol)))


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
; 3) GUI
;--------------------------------------------------------

;==============================================================
; create hierarchical menu to choose a package or a system
;==============================================================
; 1) sorted lists of strings 
;---------------------------------------
;                     .. / systemname-.... /
;                        / cffi_0.19.0 /     und diverse andere
; #<QL-DIST:SYSTEM zsort / zsort-20120520-git / quicklisp 2015-06-08>) 
(defun ql-system-name (sys) 
  (#~s'(-|_)[^-_]+?(-git|-darcs|-svn|-http|-hg)?$'' 
   (second (#~d' / ' (princ-to-string sys))))) ; ev ql:system-name


;----------------------------------
; ("ABC" ...)
(defun current-packages ()  
  (cons "common-lisp" 
        (sort 
          (remove-if-not 
            (lambda (x) (ignore-errors (asdf:find-system (pkg2sys x)))) 
            (mapcar 'package-name (list-all-packages)))
          'string<)))

(defun current-packages ()  
  (cons "COMMON-LISP" 
        (sort 
          (remove-if-not 
            ;(lambda (x) (ignore-errors (asdf:find-system (pkg2sys x)))) 
            'pkg-symbols
            (mapcar 'package-name (list-all-packages)))
          'string<)))

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
  (sort (remove-duplicates (mapcar 'ql-system-name (ql:system-list)) :test 'string=) 
        'string<))

#+quicklisp
(defun quicklisp-systems () 
  (sublis '(("cl-mssql" . "mssql"))  ; (old . new)
  (sort 
    (remove-duplicates (mapcar 'ql-system-name (ql:system-list)) :test 'string=) 
    'string<)
  :test 'equal))



; ("abc" ...)
#+quicklisp
(defun local-systems ()
  (if (probe-file my-project-dir) (push my-project-dir ql:*local-project-directories*))
  (sort (ql:list-local-systems) 'string<))

; 2) create hierarchical menu to choose a package or a system. 
;    Hierarchy by symbol-name: com. cl- asdf/ ...
;----------------------------------------------------------------------------------------
(defun create-menu (l)
  "turn a list into a sorted numbered list"
  (create-menu% (hierarchy-by-symbolname l)))

(defun create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
    (mapcar (lambda (x)
              (if (atom x)
                (list (lol:mkstr (incf n) #\space x) :value x)
                (prog1 (cons (lol:mkstr  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (1- (+ n (length x))))))) ; geht ~gut
;                (prog1 (cons (lol:mkstr (text-style-width n)??  #\space (car x)) (cons :items (list (create-menu% (cdr x))))) (setf n (1- (+ n (length x))))))) 
            l))

(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) (princ (string-downcase (car item)) strm))   ; stream-increment-cursur-position (stream-string-width  n)   <---
    (princ (string-downcase (car item)) strm)))

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


