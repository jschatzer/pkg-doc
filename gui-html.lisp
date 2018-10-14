;;;; gui-html.lisp

;from html-pkg-doc  <-------

(in-package #:pkg-doc)
(named-readtables:in-readtable h:hh)
(markup:enable-markup-syntax)

;--------------------------------------------------------
; 0) MENU BAR
;--------------------------------------------------------
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

;(defun item2li (s)
(defun sys2li (s)
  "transform an string-item into an html-list-item, without li tags"
  #M(:div s))

;(defun sys2li (s) (#~s'(s)' \1 ' s))
;(defun sys2li (s) s)
;(defun sys2li (s) #M(:p s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;(defun html-create-menu (l)
(defun create-menu-html (l)
  "turn a list into a sorted numbered list"
  ;(html-create-menu% (pkg-doc::hierarchy-by-symbolname l)))
  (html-create-menu% (pkg-doc::hierarchy-by-name l)))



(defun html-create-menu% (l &aux (n 0))
  "insert :items and :value into a tree to create a clim-menu"
  (mapcar (lambda (x)
            (if (atom x)
              (lol:mkstr (incf n) #\space x)
              ;(prog1 (cons (lol:mkstr  #\space (car x)) (list (html-create-menu% (cdr x)))) (setf n (1- (+ n (length x)))))))
              (prog1 (cons (lol:mkstr  #\space (car x)) (html-create-menu% (cdr x))) (setf n (1- (+ n (length x)))))))
          l))

;--------------------------------------------------------
; 0) PKG TREE ???
;--------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; helper and data for pkgtree
(defun item2li (s)
  "transform an string-item into an html-list-item, without li tags"
  ; item = text; info = href
  ;(destructuring-bind (item &optional info) (reverse (#~d/#\|/ s))
  (destructuring-bind (item &optional info) (reverse (#~d'\s*§\s*' s))
    (if info
      #M(:a :href info item)
      #M(:a item))))

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

;; data ;;;;;;;;;;
;(h:p lst (pkg-doc:pkg-tree "CL-FAD"))
(h:p lst (pkg-doc:pkg-tree (h:random-elt (pkg-doc:current-packages))))

;;;;;
;--------------------------------------------------------
; 0) WINDOW
;--------------------------------------------------------

;(route-html /pkgdoc19 ()    ;from html-pkg-doc
(route-html /pkgdoc0 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
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
                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

(route-html /pkgdoc1 ()   ;    is the same as  ;(route-html /pkgdoc19 ()    ;from html-pkg-doc
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
                                                          (cond ((equal (y.item.text) "packages") (lisp (sys-tree2ul (create-menu-html (cons "MENUpackages" (pkg-doc::current-packages))) "menu")))
                                                                ((equal (y.item.text) "quicklisp") (lisp (sys-tree2ul (create-menu-html (cons "MENUquicklisp" (pkg-doc::quicklisp-systems))) "menu")))
                                                                ((equal (y.item.text) "local-libs") (lisp (sys-tree2ul (create-menu-html (cons "MENUlocal-libs" (pkg-doc::local-systems))) "menu"))))))
                                          ($ "#menu" (menu (o select (f (a b) ($ "#pkg" (text (b.item.text))))))))))))
  (js ($ "#content" (height (- window.inner-Height 100)) (split (o orientation "vertical" position "50%" limit 10)))))

;geht nicht
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

;  ;;;;;;;;;;;;;;;;;;
;  
;--------------------------------------------------------
; 0) TREE PANE
;--------------------------------------------------------

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;--------------------------------------------------------
; 0) INFO PANE
;--------------------------------------------------------

(route-html /src/info ()
  ((i-js "https://code.jquery.com/jquery-1.12.4.js")  
   (i-js "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
   (i-css "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css")
   ;(i-css "/resources/demos/style.css")
   )
(:div :id "hello"))


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
