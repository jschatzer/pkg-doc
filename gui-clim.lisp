;;;; gui-clim.lisp

(in-package #:pkg-doc)
(named-readtables:in-readtable h:hh)

;==============================================================
; 0) SYS-INFO
;==============================================================
(defmacro sys-info-clim (s pkg)
  "system description"
  `(let* ,sys-info
    (format s "Nicknames: ~{~(~a~)  ~}~%" nick)  ; example with more nicknames: clazy
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

;==============================================================
; 3) GUI
;==============================================================
;create node- and leaf-classes, and corresponding methods
(cw:inf-meth :nc node-pkg
             :nn (let ((n (cw:sup cw:n))) (if (#~m':-$' n) (#~s'-$'' n) (#~s'.+:-'' n)))
             :ln (#~s'.+:-'' (cw:sup cw:n)))

(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree-pane :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info-pane :application :display-function 'disp-info :incremental-redisplay t :end-of-page-action :allow))
  (:layouts (double (horizontally () tree-pane (make-pane 'clim-extensions:box-adjuster-gadget) info-pane))))

(add-menu-item-to-command-table 'pkg-doc "textsize" :command 'txt-size) ;not working <---

(defun disp-info (f p) 
  (declare (ignore f))
  (let* ((pkg (cw:item-name (cw:group *application-frame*)))
         (inf-ap-fr (info *application-frame*))
         (sym (find-symbol (string-upcase inf-ap-fr) pkg)))
    (flet ((doc-stg (f)
             (with-drawing-options (p :text-face :bold) (format p "~2%Documentation String:~%"))
             (princ (or (manifest::docs-for sym f) "no-doc-string") p)))
      (dolist (what manifest::*categories*)
        (when (manifest::is sym what) 
          (cond 
            ((#~m'^Help' inf-ap-fr) (with-drawing-options (p :ink +blue+) (format p (info *application-frame*))))
            ((string= inf-ap-fr pkg) (sys-info-clim p pkg))
            ((member what '(:function :macro :generic-function :slot-accessor)) 
             (with-drawing-options (p :text-face :bold) (format p "~@:(~a~):~a~2%Argument List:~%" pkg sym))
             (color-lambda p (repl-utilities:arglist sym))
             (unless (null sym) (doc-stg what)))
            ((member what '(:variable :class :constant :condition)) 
             (unless (null sym) (doc-stg what)))
            (t "there could be other documantation??")))))))

(defun color-lambda (s l)
  "color lambda list"
  (mapc (lambda (x)
          (if (#~m'^&' x)
            (with-drawing-options (s :ink +red+ :text-face :bold) (format s "~(~a~)" x))
            (format s "~(~a~)" x)))
        (#~d'(&[^ )]+)'r (princ-to-string l))))

(defun tview (tree key)
  (cw-utils::t2h-r tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

;==============================================================
; 0) MENU BAR
;==============================================================
; 2) create hierarchical menu to choose a package or a system. 
;    Hierarchy by symbol-name: com. cl- asdf/ ...
;----------------------------------------------------------------------------------------
;(defun create-menu (l)
#;(defun create-menu-clim (l)
  "turn a list into a sorted numbered list"
  (create-menu% (hierarchy-by-name l)))

; 1.4.19
(defun create-menu-clim (l)
  "turn a list into a sorted numbered list"
  (create-menu% (pack l)))


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
    (princ 
      (let ((x (string-downcase (car item)) ))
        (subseq x 0 (min 25 (length x))))     ; truncate menu item-length, 31.3.19
      strm)))


;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;==============================================================
; 4) GUI COMMANDS
;==============================================================
(define-pkg-doc-command (packages :menu t) ()
  (select-pkg (current-packages)))

#+quicklisp
(define-pkg-doc-command (quicklisp :menu t) ()
  (select-pkg (quicklisp-systems)))

#+quicklisp
(define-pkg-doc-command (local-projects :menu "LocalLibs") ()
  (select-pkg (local-systems)))

; style warning; The variable PKG is defined but never used
(defun select-pkg (system-category)
  (let ((pkg (string-upcase (menu-choose (create-menu-clim system-category) 
                                         :printer 'print-numbered-pkg :n-columns 6))))     ; 5 haben nicht platz, es werden dzt nur 4 angezeigt, ql geht nur bis s...., 31.3.19
     #+quicklisp(load-package pkg)))

; style warning: The variable SYS is defined but never used
(defun load-package (p) 
  (let ((pkg (sys2pkg p))
        (sys (pkg2sys p)))
    (and (or (find-package pkg) #+quicklisp(ql:quickload sys)) 
         (create-tview  pkg))))

(defun load-package (p) 
  (let ((pkg (sys2pkg p))
        (sys (pkg2sys p)))
    (and (or (find-package pkg) #+quicklisp(ignore-errors (ql:quickload sys)))
         (create-tview  pkg))))

(defun load-package (p) 
  (let ((pkg (sys2pkg p))
        (sys (pkg2sys p)))
    (or (find-package pkg) #+quicklisp(ql:quickload sys)) 
         (create-tview  pkg)))

;(find-package "ASDF-PACKAGE-SYSTEM")  ; nil
(defun load-package (p) 
  (let ((pkg (sys2pkg p))
        (sys (pkg2sys p)))
    (or (find-package pkg) #+quicklisp(ignore-errors (ql:quickload sys)))
         (ignore-errors (create-tview  pkg))))


;; 2.4.19 ev pkg-tree-a   für alfabet sort
(defun create-tview (pkg)
  (cw-utils::t2h-r (pkg-tree pkg))
  (with-application-frame (f) 
    (setf (cw:group f) (make-instance 'node-pkg :sup (package-name pkg) :disp-inf t)) 
    (redisplay-frame-panes f :force-p t)))

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;(define-pkg-doc-command (alfabet :menu t) ()
; ;;;;;;;;;; (setf (info *application-frame*) (format t "~{~&  ~a~}" (sort *modules* 'string<))))
; use toggle pkg-tree pkg-tree-a   function call  <----
;)

;---------
(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

(define-pkg-doc-command (cl-apropos :menu t) () ; common-lisp apropos
  (setf (info *application-frame*) 
        (apropos (accept 'string) (accept 'string :default nil) 'external-only)))

#+quicklisp
(define-pkg-doc-command (ql-apropos :menu t) () ; quicklisp apropos
  (setf (info *application-frame*) (ql:system-apropos (accept 'string))))

(define-pkg-doc-command (help :menu t) ()
  (with-drawing-options (t :ink +blue+) (princ *help* *standard-output*)))

(define-pkg-doc-command (clear :menu t) ()
  (window-clear *standard-input*))

(define-pkg-doc-command (features :menu t) ()
  (setf (info *application-frame*) 
  (with-drawing-options (t :ink +red+) (format t "~{~&  ~a~}" (sort *features* 'string<)))))

(define-pkg-doc-command (modules :menu t) ()
  (setf (info *application-frame*) (format t "~{~&  ~a~}" (sort *modules* 'string<))))

(defvar *help* "Help:
Click the root node to see a package's description or readme-file
-----------------------------------------------------------------
APROPOS: 
1) on the first prompt type a string
2) on the second promt press enter or type the name of a package
-----------------------------------------------------------------
CONFIGURE-POSSIBILITIES:
1) adapt local-libs
;;2) (setf clim-pkg-doc::*st* :a)  ;to change the symbol-type  :e external(default) :p resent :a available ???
;;3) all symbols alfabetically ??
-----------------------------------------------------------------
")

;==============================================================
; 5) MAIN
;==============================================================
(defun pkg-doc (&optional (pkg "CLIM")) 
 (tview  (pkg-tree pkg) pkg))

(defun pd () (clim-sys:make-process #'pkg-doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
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
|#

#;(defun print-numbered-pkg (item strm)
  (if (#~m'[-./]$' (car item))
    (with-drawing-options (strm :ink +red+ :text-face :bold) (princ (string-downcase (car item)) strm))   ; stream-increment-cursur-position (stream-string-width  n)   <---
    (princ (string-downcase (car item)) strm)))


