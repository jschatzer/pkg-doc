;;;; gui-html.lisp
;copy from fiel 8.4.19

;https://stackoverflow.com/questions/6316979/selecting-an-element-in-iframe-jquery
; $("#containerdiv div").draggable( {containment: "#containerdiv ", scroll: false} );   <----- anschauen

;from html-pkg-doc  <-------

(in-package #:pkg-doc)
(named-readtables:in-readtable h:w)
(markup:enable-markup-syntax)
(cl-interpol:enable-interpol-syntax)

;==============================================================
; 0) SYS-INFO
;==============================================================
; simple text, ev html+color
#;(defmacro sys-info-html (s pkg)
  `(let* ,sys-info
    (format s "Nickname: ~{~a~}~%" nick)
    (format s "~a " nr) (format s "external-symbols~%")
    (format s 
"-------------------------
 Package Documentaiton String
-------------------------~2%")
    (format s 
"-------------------------
 ASDF Description
-------------------------")
    (format s "~&SHORT: ") (format s "~a" a1)
    (format s "~2&LONG: ") (format s "~a~%" a2)
    (format s 
"-------------------------
 README
-------------------------")
    (format s "~&~a" a3)))


;as a string, geht
(defmacro sys-info-html (pkg &optional s)
  `(let* ,sys-info
    (format nil "Nickname: ~{~a~}~%~a external-symbols~%
-------------------------
 Package Documentaiton String
-------------------------~2%
-------------------------
 ASDF Description
-------------------------
~&SHORT: ~a
~2&LONG: ~a~%
-------------------------
 README
-------------------------
~&~a"  nick nr a1 a2 a3)))

;==============================================================
; 0) MENU BAR
;==============================================================
;;;; navigation ;;;;;;;;;;;;;;;;;;;;;;;;;,
;geht vorerst
(defun -list2ul (l)
  (h:lststg 
    (loop for x in l collect #M(:li (sys2li x)))))

(defun list2ul (l &optional (id ""))
  #M(:ul :id id (-list2ul l)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sys-tree2ul (tr)
  #M(:ul :id "menu"
         (-sys-tree2ul tr)))

(defun sys-tree2ul (tr &optional (id ""))
  #M(:ul :id id
         (-sys-tree2ul tr)))


(defun -sys-tree2ul (tr)
  (let* ((item-strg (if (atom tr) tr (car tr)))
         (item (sys2li item-strg)))
    (if (consp tr)
      #M(:li item
             (:ul (loop for x in (mapcar '-sys-tree2ul (cdr tr)) collect x)))
      #M(:li item))))

;use :a ancor statt :div ??  ---  idem .tree
;(defun item2li (s)
(defun sys2li (s)
  "transform an string-item into an html-list-item, without li tags"
  #M(:div s))

;(defun sys2li (s) (#~s'(s)' \1 ' s))
;(defun sys2li (s) s)
;(defun sys2li (s) #M(:p s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
(defun create-menu-html (l)
  "turn a list into a sorted numbered list"
  (create-menu-html% (hierarchy-by-name l)))

(defun create-menu-html% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
  (mapcar (lambda (x)
            (if (atom x)
              (lol:mkstr (incf n) #\space x)
              (prog1 (cons (lol:mkstr  #\space (car x)) (create-menu-html% (cdr x))) (setf n (1- (+ n (length x)))))))
          l))

;==============================================================
; 0) PKG TREE ???
;==============================================================
;;;; helper and data for pkgtree
(defun item2li (s)
  "transform an string-item into an html-list-item, without li tags"
  ; item = text; info = href
  ;(destructuring-bind (item &optional info) (reverse (#~d/#\|/ s))
  (destructuring-bind (item &optional info) (reverse (#~d'\s*§\s*' s))  ; geht irgendwie, hatte ich bisher
  ;;;;;(destructuring-bind (item &optional info) (reverse (#~d'\s*:-\s*' s))  ; geht ~gut
    (if info
      #M(:a :href info item)
      #M(:a item))))

;6.4.2019
(defun item2li (s)
  "transform an string-item into an html-list-item, without li tags"
  ; item = text; info = href
  ;(destructuring-bind (item &optional info) (reverse (#~d/#\|/ s))
  ;(destructuring-bind (item &optional info) (reverse (#~d'\s*§\s*' s))  ; geht irgendwie
;  (let ((item (or (second (#~d':-' s)) (first (#~d':-' s)))))  ; geht ~gut
;    (if info
;(let ((item (or (second (#~d':-' s)) s)))
(h:aif (second (#~d':-' s))
;      #M(:a :href s h:it)
;      #M(:a :href h:it h:it)
      #M(:a h:it)
      #M(:a (#~s'-$'' s))))


(defun -tree2ul (tr)
  (let* ((item-strg (if (atom tr) tr (car tr)))
         (item (item2li item-strg)))
    (if (consp tr)
      #M(:li item (:div :class "expander" "") 
             (:ul (loop for x in (mapcar '-tree2ul (cdr tr)) collect x)))
      #M(:li item))))

(defun tree2ul (tr)
  #M(:ul :class "tree"
         (-tree2ul tr)))

;==============================================================
; 0) WINDOW
;==============================================================
;(route-html /pkgdoc19 ()    ;from html-pkg-doc
; siehe gui-html.8.4.19.lisp for more
;nur pkgdoc4, geht ~gut,  
;und 9 , geht nicht gut
;und 10 zum testen behalten
;ladet langsam wegen warnings dot operator

;geht prinzipiell
#;(route-html /pkgdoc4 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
;        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ($ "#pkg" 
                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
                                                                           ;($ ("#content" (c (p top.frames "tree") document)) 
                                                                           (load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))


;remove dot operator, geht
(route-html /pkgdoc4 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
;        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (c y item (text)) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (c y item (text)) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (c y item (text)) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ($ "#pkg" 
                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
                                                                           ;($ ("#content" (c (p top.frames "tree") document)) 
                                                                           (load (s "/src/pkgtree?pkg=" (c b item (text) (replace (r "/^\\d*\\s*/") "")))))))))))))
      ($ "#content" (height (- (c window inner-Height) 100)) (split (o orientation "vertical" position "50%" limit 10)))))

;old
; return jQuery('#pkg').load(['/src/pkgtree?pkg=', b.item.text().replace(/^\d*\s*/, '')].join(''));

;new
; return jQuery('#pkg').load(['/src/pkgtree?pkg=', b.item.text.replace(/^\d*\s*/, '')].join(''));

#|
(route-html /pkgdoc9 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg" "")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ($ "#tree" (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

; 8.4.19
(route-html /pkgdoc10 ()
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  ;(list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules" "navigation"))  ;???
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg" "")
  ;(i-js jq)
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")

     ;(js ($ ("#info" (c (p top.frames "info") document)) (:style "background:red")))
(js 
  ;(w:doc-ready
  ;($ "iframe#info" (contents) (find "#hello") (css "background-color" "red"))
  ($ "iframe#info" (contents) (find "#hello") (text "background-color"))
  )

  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))

                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ($ "#tree" (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") "")))))))))))))
      ;; test
      ;($ ("#tree" (c (p top.frames "tree") document)) (:style "background:red"))
      ;..........
      ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))
|#


;; ; ;($ ("#tree" (c (p top.frames "tree") document)) 
;; ; ;($ ("#content" (c (p top.frames "tree") document)) 
;; ; 
;; ; ;https://www.learningjquery.com/2016/09/using-jquery-to-access-iframe-elements
;; ; $(document).ready(function(){
;; ; var iFrameDOM = $("iframe#frameID").contents();
;; ; 
;; ; iFrameDOM.find(".page").css("background-color", "#fff");
;; ; 
;; ; ;($ "iframe#info" (content) (find "body") (css "background-color" "red"))
;; ; 
;; ; ;--------------------------------



;==============================================================
; 0) TREE PANE
;==============================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;geht mit pkgdoc1
(let ((pkg (h:random-elt (pkg-doc:current-packages))))
  (route-html /src/pkgtree ()
    ((i-css "/jquery-simplefolders/main.css")
     (css (li white-space nowrap) 
          ("a:hover" background-color "#81F7BE")))
    ;  (tree2ul lst)
    (tree2ul (pkg-doc:pkg-tree pkg))

    (i-js jqm)
    (i-js "/jquery-simplefolders/main.js")
    (js ($ ".tree" (on "click" "a" (f0 
                                     ($ ("#hello" (c (p top.frames "info") document)) 
                                        ;                                        (text ($ this (text))))))))))
                                        ;(text (ps:stringify "/arg-list?sym=" (c ($ this (text)) (replace (ps:regex "/.*:-/") "")) "&pkg=" (lisp pkg))))))))))
                                        (load (ps:stringify "/arg-list?sym=" (c ($ this (text)) (replace (ps:regex "/.*:-/") "")) "&pkg=" (lisp pkg))))))))))
|#

;-------------------------------------------------
; 6.4.19
#;(route-html /src/pkgtree (pkg)
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul (pkg-doc:pkg-tree pkg))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                      (load (ps:stringify "/arg-list?sym=" ($ this (text)) "&pkg=" (lisp pkg)))))))))

(route-html /src/pkgtree (pkg)
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul (pkg-doc:pkg-tree pkg))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p (c top frames) "info") document)) 
                                      (load (ps:stringify "/arg-list?sym=" ($ this (text)) "&pkg=" (lisp pkg)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;==============================================================
; 0) INFO PANE
;==============================================================
;keep format
(route-html /arg-list (sym pkg)
  ()
  #M(:pre (:code (with-output-to-string (*standard-output*) (h:d (find-symbol (string-upcase sym) pkg))))))

(route-html /src/info ()
  ((i-js "https://code.jquery.com/jquery-1.12.4.js")  
   (i-js "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
   (i-css "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css")
   ;(i-css "/resources/demos/style.css")
   )
(:div :id "hello"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
@END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;MENU https://jqueryui.com/menu/#default
#;(route-html /src/info ()
  ((i-js "https://code.jquery.com/jquery-1.12.4.js")  
   (i-js "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
   (:script 
     #>END
     $( function() {
                   $( "#menu" ).menu();
                   } );
     END)
   (i-css "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css")
   (i-css "/resources/demos/style.css")
   ;(css (".ui-menu" :width 150px))
   )
(:div :id "hello" "hellllllo")
#"
<ul id="menu">
<li class="ui-state-disabled"><div>Toys (n/a)</div></li>
<li><div>Books</div></li>
<li><div>Clothing</div></li>
<li><div>Electronics</div>
<ul>
<li class="ui-state-disabled"><div>Home Entertainment</div></li>
<li><div>Car Hifi</div></li>
<li><div>Utilities</div></li>
</ul>
</li>
<li><div>Movies</div></li>
<li><div>Music</div>
<ul>
<li><div>Rock</div>
<ul>
<li><div>Alternative</div></li>
<li><div>Classic</div></li>
</ul>
</li>
<li><div>Jazz</div>
<ul>
<li><div>Freejazz</div></li>
<li><div>Big Band</div></li>
<li><div>Modern</div></li>
</ul>
</li>
<li><div>Pop</div></li>
</ul>
</li>
<li class="ui-state-disabled"><div>Specials (n/a)</div></li>
</ul>
"#


  (:div :id "title")
(:div :id "hello"  (:pre (:code
  (with-output-to-string (s)
    (pkg-doc::pkg-description s "CL-FAD")))))
  (i-js jqm)
(:script
 " 
$(document).ready(function(){  
  $('#title').html($('title').html()); 
}); 
"))


 
#| 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; info- tree.html
; <li><a id="<pre><code>Iniezione di sieri immuni</code></pre>">99.14 Iniezione di gamma globuline</a></li>
;;;;;;icdhtml
  <script>
  $(document).ready(function(){
                     //  $('ul').on('click', 'li', function(){
                                    $('ul').on('click', 'a', function(){
                                               $('#hello',top.frames["info"].document)
                                               .html($(this).attr('id'));
                                               });
                                    });
                     </script>

Info,html
<div id="hello" >
|#



;(route-html /home-tree ()
;(route-html /pkgdoc ()
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (:h1 "Homepage")
  ;(tree2ul lst1)
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  ;(i-js "/simple-folders")
;;;;;;$( "#result" ).load( "ajax/test.html" );
;;;   $(selector).load(URL,data,callback);  <---- https://www.w3schools.com/jquery/jquery_ajax_load.asp  gute Beschreibung
(:script ;geht
"$('ul').on('click', 'a', function(){ $('#hello',top.frames['info'].document)
           .load('/info')
//             .load('/info', { sym: 'cl-fad'})
//              .load('/info', 'cl-fad')
         
            
            });
"))

#|
$('ul').on('click', 'a', function(){
                 $('#hello',top.frames["info"].document)
                        .html($(this).attr('id') ? '<pre><code>' + $(this).attr('id') + '</code></pre>' : '');
                            });
;-------------
 #;(:script
   "
$('ul').on('click', 'a', function(){
                 $('#hello',top.frames['info'].document)
//.html( $.get('/info', function (_) {return _;}))});
.html( $.get('/info', function (_) { return '<pre><code>' +  _ + '</code></pre>';}))});

//.html( $.get('/info', function (_) {console.log( _ );}))});

")
|#

;-----------------------------------------
;scheint zu gehen
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (:h1 "Homepage")
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ "ul" (on("click" "a" (f0 ($ ("#hello" top.frames["info"].document)
           (load "/info"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;use jqui selcteble statt jq on  <---??
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (:h1 "Homepage")
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
(js ($ "#tree" (on "click" "#tree div" (f0 
  ($ ("#hello" (c (p top.frames "info") document)) (html "Hakg jlag")))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
 ; das geht
 (route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (:h1 "Homepage")
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
(js 
  ($ ("#hello" (c (p top.frames "info") document)) (text "Hakg jlag"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
(js 
  ($ ("#hello" (c (p top.frames "info") document)) (text (lisp (h:stg (repl-utilities:arglist 'clim:present)))))))

(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "li" (f_  
 ;                                   (console.log _.target.text-content)
                                     (console.log ($ this (text)))
                                   
                                    )))))

;;;;;;;;;;;;;; geth gut ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 (console.log ($ this (text))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;geht nicht, test load pkg
(route-html /pkgdoc0x ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css 
     ("#navigation.ui-menu .ui-menu-item" float left)
     ;     ("#content #menu" width 200px)
     ("#menu" width 200px)
     ;     ("#menu" columns 4)    
     ("#content ul" columns 2)     ; geht prinzipiell
     )
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (html-create-menu (cons "MENU" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (html-create-menu (cons "MENU" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (html-create-menu (cons "MENU" (pkg-doc::local-systems))) "menu"))))))
 ;                                         ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text)))))))
                                           ($ "#menu" (menu (o select (f (a b) (load (format nil "/src/pkgtree?~a" (b.item.text)))))))
                                         
                                          )))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))



; (js ($ "#menu" (menu (o select (f (x y) ($ "#pkg" (text (elt (c (y.item.text) (split " ")) 0))))))))




#|
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                        (text (lisp (h:stg (repl-utilities:arglist (find-symbol (string-upcase (#~s'function:-'' ($ this (text)))))))))))))))

|#


                                   


;                               ($ ("#hello" (c (p top.frames "info") document)) 
;                                  _.target.text-content
;                                  ))))))

;(html "hellllo")
;          (html ($ this (text))) 
;           )))))))

;;;;; test
#;(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (:h1 "Homepage")
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
(js  ($ "#tree" (on "click" "#tree li" (f0
                    (alert ($ this (text))))))))
  
  
;  ($ "#tree" (on "click" "#tree div" (f0 
;  ($ ("#hello" (c (p top.frames "info") document)) (html "Hakg jlag")))))))
 

;.html($(this).attr('id'));


#;(route-txt /arg-list (sym)
(h:stg (repl-utilities:arglist (find-symbol (string-upcase sym)))))

#;(route-txt /arg-list (sym)
(h:stg (repl-utilities:arglist (intern (string-upcase sym)))))

#;(route-txt /arg-list (sym)
(h:stg (repl-utilities:doc (find-symbol (string-upcase sym)))))

(route-html /pkgdoc1 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

(route-html /pkgdoc2 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
                                          ;($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text)))))))

                                          ;($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text.replace (ps:regex "/\d+\s+/") "")))))))
                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text.replace (ps:regex #?r"/\d*\s*/") "")))))))

                                         ;($ ("#tree" (c (p top.frames "tree") document)) (html (o source (+ "/src/pkgtree?pkg=" (b.item.text.replace (ps:regex "\d+\s+") "")))))

                                           ;($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text)))))))
                                         
                                          )))))

  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))


;(autocomplete (o source "/get-symbols"))

#;(route-html /pkgdoc3 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") "")))))))))))))  ; geht

  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

;geht nicht
(route-html /pkgdoc5 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ;($ "#pkg" 
                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
                                                                           ($ "#content #tree" ;(c (p top.frames "tree") document)) 
;                                                                           ($ ("body" (c (p top.frames "tree") document)) 
                                                                           (load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

(route-html /pkgdoc6 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg" "")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ;($ "#pkg" 
                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
                                                                           ;($ "#content #tree" ;(c (p top.frames "tree") document)) 
;                                                                           ($ ("body" (c (p top.frames "tree") document)) 

;                                                                           ($ "#tree" (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
;                                                                           ($ "div iframe#tree" (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
;                                                                           ($ (c (p top.frames "tree") document) (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
;                                                                           ($ "#tree" (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

;                                                                           ($ ("#tree" (c (p top.frames "tree") document)) (attr "src" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
                                                                           ($ ("#tree" (c (p top.frames "tree") document)) (attr "prop" (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

;                                                                         ($ ("#tree" (c (p top.frames "tree") document))   (load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

                                                                           ;(load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

#|
<script>
  $('#changeframe').click(function () {
                                         $('#declinedframe').attr('src', 'http://stackoverflow.com');
                                           });
  </script>
|#

(route-html /pkgdoc7 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
;        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg" "")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) 
;                                                                        ($ "#pkg" 
;                                                                         ($ "#content" 
                                                                         ;($ "#content #tree" 

;                                                                           ($ ("html" (p top.frames "tree")) 
                                                                           ($ ("#tree" (p top.frames "tree")) 

                                                                           ;($ ("#content" (c (p top.frames "tree") document)) 
                                                                           (load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

(route-html /pkgdoc8 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
  ((i-css jts)
   (i-css "/jquery.splitter/css/jquery.splitter.css")
   (css ("#navigation.ui-menu .ui-menu-item" float left)
     ("#menu" width 200px)
     ("#content ul" columns 2))     ; geht prinzipiell
   (css (".splitter_panel .vsplitter" background-color gray width 3px)))
  (list2ul '("packages" "quicklisp" "local-libs" "cl-apropos" "ql-apropos" "help" "features" "modules") "navigation")
  (:br)
  (:div :id "content"
;        (:iframe :src "/src/pkgtree" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/pkgtree?pkg=CLIM" :name "tree" :id "tree" :marginwidth "0" :marginheight "0" :scrolling "yes" "") ;there must be content, to have a closing tag
        (:iframe :src "/src/info" :name "info" :id "info" :marginwidth "0" :marginheight "0" :scrolling "yes" ""))
  (:div :id "pkg" "")
  (i-js jq3.3)
  (i-js ui1.12)
  (i-js "/jquery.splitter/js/jquery.splitter.js")
  (js ($ "#navigation" (menu (o select (f (x y) 
                                          ($ "#content" (html 
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
;                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
                                          ($ "#menu" (menu (o select (f (a b) 
                                                                        ;($ "#pkg" 
;                                                                           ($ ("#tree" ($ "iframe" (contents)))
;($ "iframe#tree"
;($ "#content iframe"                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
 ($ "#content iframe #tree"                                                                           ;($ ("#tree" (c (p top.frames "tree") document)) 
                                                                          ;($ ("#content" (c (p top.frames "tree") document)) 
                                                                           (load (ps:stringify "/src/pkgtree?pkg=" (c (b.item.text) (replace (ps:regex "/^\\d*\\s*/") ""))))))))))))))

  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

;Another syntax: $("textarea", $("iframe").contents()).keydown(...) –






;/src/pkgtree?pkg=CL-FAD

;(c ($ this (text)) (replace (ps:regex "/.*:-/") ""))
;
;-------------------------------------------------------
#| clim display function
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
|#
;-----------------------------------------------------------------

#;(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  ;(let ((pkg (h:random-elt (pkg-doc:current-packages))))
  ;  (tree2ul (pkg-doc:pkg-tree pkg))
  ;(tree2ul lst)
  (tree2ul (pkg-doc:pkg-tree "CL-FAD"))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                      (text ;(if (equal ($ this (text)) (lisp pkg))
                                              (lisp (sys-info-clim *standard-output* pkg))                    
                                              ;($ this (text))
                                              ;"hello"
                                              )))))))



;(if (string= 
; ((string= inf-ap-fr pkg) (sys-info-clim p pkg))

; sys-info-html (s pkg)

#|
;;;;;;;;
;geht ;;; http://acer/src/pkgtree?pkg=CL-FAD
;optional geht nicht??
;(route-html /src/pkgtree (&optional (pkg "CL-FAD"))
(route-html /src/pkgtree (pkg)
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (tree2ul lst)
  (tree2ul (pkg-doc:pkg-tree pkg))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                        (text ($ this (text)))))))))

|#

;(h:p lst (pkg-doc:pkg-tree "CL-FAD"))

;;;; geht
(route-html /src/pkgtree (pkg)
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul (pkg-doc:pkg-tree pkg))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                      (load (ps:stringify "/arg-list?sym=" (c ($ this (text)) (replace (ps:regex "/.*:-/") "")) "&pkg=" (lisp pkg)))))))))
; only arglist
(route-html /arg-list (sym pkg)
  ()
  (h:stg (repl-utilities:arglist (find-symbol (string-upcase sym) pkg))))

;6.3.19 geht auch, describe
(route-html /arg-list (sym pkg)
  ()
  (with-output-to-string (*standard-output*) (h:d (find-symbol (string-upcase sym) pkg))))


;; data ;;;;;;;;;;
;(h:p lst (pkg-doc:pkg-tree "CL-FAD"))
(h:p lst (pkg-doc:pkg-tree (h:random-elt (pkg-doc:current-packages))))

;geht gut
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   ($ ("#hello" (c (p top.frames "info") document)) 
                                        (text ($ this (text)))))))))

;test  edit
;; regex mit cl-interpol s 6 ps
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
  (tree2ul lst)
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   (let ((item ($ this (text))))
                                     ($ ("#hello" (c (p top.frames "info") document)) 
                                        ;(text (+ item " -- " item " -- " (c item (replace (ps:regex "/.*:-/") "")))) ; geht
                                        (text (+ item " -- " item " -- " (item.replace (ps:regex "/.*:-/") ""))) ; geht

;                                        (text (h:stg (repl-utilities:arglist (item.replace (ps:regex "/.*:-/") ""))))

                                        )))))))

(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (tree2ul lst)
  (tree2ul (pkg-doc:pkg-tree "CL-FAD"))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   (let ((item ($ this (text))))
                                     ($ ("#hello" (c (p top.frames "info") document)) 
;                                        (load (+ "/arg-list?sym=" (item.replace (ps:regex "/.*:-/") ""))))))))))
                                        (load (s "/arg-list?sym=" (item.replace (ps:regex "/.*:-/") ""))))))))))


                                        ;(text (+ item " -- " item " -- " (c item (replace (ps:regex "/.*:-/") "")))) ; geht
;                                        (text (+ item " -- " item " -- " (item.replace (ps:regex "/.*:-/") ""))) ; geht

;                                        (text (h:stg (repl-utilities:arglist (item.replace (ps:regex "/.*:-/") ""))))

;                                        )))))))

;(pkg-doc:pkg-tree (h:random-elt (pkg-doc:current-packages)))

(let ((pkg (h:random-elt (pkg-doc:current-packages))))
(route-html /src/pkgtree ()
  ((i-css "/jquery-simplefolders/main.css")
   (css (li white-space nowrap) 
        ("a:hover" background-color "#81F7BE")))
;  (tree2ul lst)
  (tree2ul (pkg-doc:pkg-tree pkg))
  (i-js jqm)
  (i-js "/jquery-simplefolders/main.js")
  (js ($ ".tree" (on "click" "a" (f0 
                                   (let ((item ($ this (text))))
                                     ($ ("#hello" (c (p top.frames "info") document)) 
                                        (html pkg)))))))))

;                                        (load (+ "/arg-list?sym=" (item.replace (ps:regex "/.*:-/") ""))))))))))
;                                        (load (s "/arg-list?sym=" (item.replace (ps:regex "/.*:-/") "")))))))))))
;                                        (load (ps:stringify "/arg-list?sym=" (c item (replace (ps:regex "/.*:-/") ""))))))))))))


