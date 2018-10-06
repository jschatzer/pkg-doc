;;;; clim.lisp

(in-package #:pkg-doc)

;--------------------------------------------------------
; 3) GUI
;--------------------------------------------------------
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
            ((string= inf-ap-fr pkg) (pkg-description p pkg))
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

;--------------------------------------------------------
; 4) GUI COMMANDS
;--------------------------------------------------------
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
  (let ((pkg (string-upcase (menu-choose (create-menu system-category) 
                                         :printer 'print-numbered-pkg :n-columns 5))))
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



(defun create-tview (pkg)
  (cw-utils::t2h-r (pkg-tree pkg))
  (with-application-frame (f) 
    (setf (cw:group f) (make-instance 'node-pkg :sup (package-name pkg) :disp-inf t)) 
    (redisplay-frame-panes f :force-p t)))

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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

;--------------------------------------------------------
; 5) MAIN
;--------------------------------------------------------
(defun pkg-doc (&optional (pkg "CLIM")) 
 (tview  (pkg-tree pkg) pkg))

(defun pd () (clim-sys:make-process #'pkg-doc))
