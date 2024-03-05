;;;; postmodern-web  - Generate a class for a table and also a input form and a select table in the browser.
;;;; 07.02.2024 John Thingstad

(defpackage postmodern-web
  (:shadowing-import-from :utils #:while)
  (:shadowing-import-from :sb-ext #:create)
  (:shadowing-import-from :parenscript #:%)
  (:local-nicknames (:u :utils) (:pu :postmodern-utils) (:a :alexandria))
  (:use  #:cl #+sbcl #:sb-ext :hunchentoot :postmodern :parenscript :spinneret :cl-css :cl-interpol)
  (:export :start-web-server :start-db-connection :start-testing :stop-testing))

(in-package :postmodern-web)

(named-readtables:in-readtable :interpol-syntax)

(compile-css
 (merge-pathnames (make-pathname :name "style" :type "css" :directory '(:relative "www"))
		  (asdf:system-source-directory "table-grinder"))
 '(("*" :box-sizing "border-box")
   ("body" :font-family "Cantarell, Ariel, Helvetice, sans-serif"
	   :font-size "22px"
	   :background-color "#F1F1F1")
   ("header" :background-color "#666"
	     :padding "30px"
	     :text-align "center"
	     :font-size "24px"
	     :color "white")
   ("section" :display "-webkit-flex" :display "flex")
   ("nav" :-webkit-flex "1" :-ms-flex "1" :flex "1"
	  :background "#ccc"
	  :padding "20px")
   ("nav menu" :list-style-type "none"
	       :padding "0")
   ("nav menu a"  :background-color "#CCC" :color "black" :text-decoration "none")
   ("nav menu a:hover"  :background-color "#777" :color "white")
   ("footer a" :background-color "#777" :color "white")
   ("footer a:visited" :color white)

   ("article" :-webkit-flex "3" :-ms-flex "3" :flex "3"
	      :background-color "#f1f1f1"
	      :padding "10px")

   ("#db th" :background-color "#CCC")
   ("#db tr:nth-child(odd)" :background-color "#DDD")
   ("#db td" :cursor "pointer")
   ("#db td.numeric" :text-align "right")
   ("#db tr:hover" :background-color "#666" :color "white")
   
   ("ul.table" :list-style-type "none")
   ("ul.table li a" :text-decoration "none" :color "black")
   ("ul.table li a:visited" :color "black")
   ("ul.table li a:hover" :background-color "#CCC" :color "white")

   (".form" :text-decoration "none" :color "black")
   (".form:visited" :color "black")

   ("footer" :background-color "#777"
	     :padding "10px"
	     :text-align "center"
	     :color "white")
   ("@media (max-width: 600px)"
    ("section" :-webit-flex-directon "column" :flex-direction "column"))))

(defun present (type col)
  (cond
    ((eql col :null) "")
    ((eql col nil) "<error>")
    ((string= type "text") col)
    ((string= type "bpchar") col)
    ((string= type "numeric")
     (format nil "~,2F" col))
    ((string= type "int2") (write-to-string col)) 
    ((string= type "int4") (write-to-string col))
    ((string= type "int8") (write-to-string col))
    ((string= type "date")
     (multiple-value-bind (year month day)
	 (simple-date:decode-date col)
       (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
    ((string= type "timestamp")
     (multiple-value-bind (year month day hour minute second millisecond)
	 (simple-date:decode-timestamp col)
       (declare (ignorable millisecond))
       (format nil "~4,'0d-~2,'0d-~,2dT~2,'0D:~2,'0d:~2,'0d" year month day hour minute second)))
    ((string= type "interval")
     (multiple-value-bind (year month day hour minute second millisecond)
	 (simple-date:decode-interval col)
       (declare (ignorable year month day millisecond))
       (format nil "~2,'0D:~2,'0d:~2,'0d" hour minute second)))))

(defun input-type (db-type)
  (cond ((string= db-type "date") "date")
	((string= db-type "timestamp") "datetime-local")
	((member db-type (list "numeric" "int2" "int4" "int8") :test #'string=) "number")
	(t "text")))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun web-store-table (post-alist session-parameters)
  (let* ((table-name (cdr (first post-alist)))
	 (index (parse-integer (cdr (second post-alist))))
	 (pre-change-up (cddr (nth index session-parameters)))
	 (table-types (mapcar #'second (table-description (intern table-name))))
	 (pre-change (loop for (name value) on pre-change-up by #'cddr
			   for type in table-types
			   collect name
			   collect (present type value)))
	 (post-change (loop for (name . val) in (cddr post-alist) ; skip table-name and index entries
			    collect (intern (string-upcase name) "KEYWORD")
			    collect val)))
    (pu:store-table table-name pre-change post-change)))

(defmacro with-form (table-name table-action index row-plist)
  `(let ((table-desc (table-description (intern ,table-name))))
     (with-html
       (:form :action ,table-action :method "POST"
	      (:table
	       (:input :type "hidden" :id "table-name" :name "table-name" :value ,table-name)
	       (:input :type "hidden" :id "index" :name "index" :value ,index)
	       (dolist (col table-desc)
		 (let* ((name (first col))
			(type (second col))
			(value (getf ,row-plist (make-keyword name))))
		   (:tr
		    (:td (:label :for name (string-upcase name)))
		    (:td
		     (if (and (string= type "text") (not (eql value :null)) (> (length value) 30))
			 (:textarea :id name :name name
				    :rows (floor (length value) 60) :cols 70
				    (present type value))
			 (:input :type (input-type type)
				 :id name :name name
				 :value (present type value))))))))
	      (:input :type "submit" :value "submit")))))

(defun make-form-plist (name table-desc row)
  (let ((plist (list :table-name name)))
    (loop for field-name in table-desc
	  for col-value in row
	  do
	     (setf plist (concatenate 'list plist (list (make-keyword (first field-name)) col-value))))
    plist))

(defun numeric? (type)
  (member type '("numeric" "int2" "int4" "int8") :test #'string=))

(defun datetime? (type)
  (member type '("date" "timestamp" "interval") :test #'string=))

(defmacro with-table (name uri)
  `(let ((table-desc (table-description (intern ,name)))
	 (rows (query (:select '* :from (intern ,name)))))
     (let ((form-data))
       (dolist (row rows)
	 (push (make-form-plist ,name table-desc row) form-data))
       (setf (hunchentoot:session-value :form-data *session*)
	     (concatenate 'list
			  (nreverse form-data)
			  (list :table-name name))))
     (with-html
       (with-ps
	 (wait-doc ; wait til page is finished drawing
	   (chain ($ "tr:gt(0)") ; assign a click callback to each row except the first
		  (click (lambda () ; set page to the element with the link <a class="form"...>'s href
			   (setf (@ window location href) (chain ($ this) (find "a.form") (attr "href"))))))))
       (:table :id "db"
	       (:tr
		(dolist (col table-desc)
		  (:th (string-upcase (first col)))))
	       (loop for row in rows
		     for index from 0 to (1- (length rows))
		     do
			(let ((type (second (first table-desc)))
			      (value (present (second (first table-desc)) (first row)))
			      (url (format nil "~A?index=~D" ,uri index)))
			  (:tr
			   (when (first row)
			     (cond ((numeric? type)
				    (:td :class "numeric" (:a :class "form" :href url value)))
				   ((datetime? type)
				    (:td  (:a :class "form" :href url (:time :datetime value value))))
				   (t (:td (:a :class "form" :href url value)))))
			   (loop for desc in (rest table-desc)
				 for col in (rest row)
				 do
				    (let ((type (second desc))
					  (value (present (second desc) col)))
				      (cond ((numeric? type) (:td :class "numeric" value))
					    ((datetime? type) (:td (:time :datetime value value)))
					    (t (:td value))))))))))))
(defmacro with-ps (&body body)
  `(with-html (:script (:raw (ps ,body)))))

(defpsmacro wait-doc (&body body)
  `(chain ($ document) (ready (lambda () ,body))))

(defmacro with-page ((&key title) &body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (hunchentoot:no-cache)
     (with-output-to-string (*standard-output*)
       (with-html
	 (:doctype)
	 (:html
	  (:head
	   (:title ,title)
	   (:meta :name "viewport" :content "width=device-width, initial-scale=1")
	   (:link :rel "icon" :type "image/x-icon" :href "/img/favicon.ico")
	   (:link :rel "stylesheet" :href "style.css")
	   ;; https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js is you don't wan't to download it
	   (:script :src "jquery-3.7.1.min.js"))
	  (:body ,@body))))))

(defmacro with-footer ()
  `(with-html
     (:footer
      (:hr)
      (:a :href "http://weitz.de/hunchentoot" "Hunchentoot")
      (if *database*
	  (format nil " ~A running on ~A ~A - ~A"
		  hunchentoot:*hunchentoot-version*
		  (lisp-implementation-type)
		  (subseq (lisp-implementation-version) 0 6)
		  (subseq (database-version) 0 16))
	  (format nil " ~A running on ~A ~A"
		  hunchentoot:*hunchentoot-version*
		  (lisp-implementation-type)
		  (subseq (lisp-implementation-version) 0 6))))))
