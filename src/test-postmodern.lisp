;;;; test-postmodern - Connects to posgresql, sets up a mock database and cleans up.
;;;;                   Also sets up a hunchentoot server and shuts it down
;;;; 08.02.2024 John Thingstad

(in-package :postmodern-web)

(defvar *database* nil)
(defvar *webserver* nil)
(defvar *session* nil)
(defparameter *webserver-port* 4242)
(defparameter *database-host* "localhost")
(defparameter *default-database* "john")
(defparameter *database-user* "john")
(defparameter *database-password* "")

;;;----------------------------------------------------------------------
;;; Webserver

(hunchentoot:define-easy-handler (restart-table-grinder :uri "/restart-table-grinder") ()
  (stop-testing)
  (start-testing)
  (hunchentoot:redirect "/"))

(hunchentoot:define-easy-handler (form-update :uri "/form-update") ()
  :request-type :POST
  (let* ((post-alist (hunchentoot:post-parameters*))
	 (table-name (cdr (first post-alist))))
    (web-store-table post-alist (hunchentoot:session-value :form-data *session*))
    (hunchentoot:redirect (format nil "/table-view?name=~A" table-name))))

(hunchentoot:define-easy-handler (cookie-error :uri "/cookie-error") ()
  (with-page (:title "Cookie Error")
    (:header
     (:h1 "Cookie Error")
     (:h2 "Session Cookie missing or expired"))
    (:section
     (:nav
      (:menu
       (:li (:a :href "/database-tables" "Database Tables"))))
     (:article
      (:p "Hunchentoot will regenerate the session cookie for you. Sending you back to Database Tables.")))
    (with-footer)))

(hunchentoot:define-easy-handler (form-view :uri "/form-view") (index)
  (handler-case
      (let* ((nindex (parse-integer index))
	     (form-data (nth nindex (hunchentoot:session-value :form-data *session*)))
	     (table-name (getf form-data :table-name))
	     (row-plist (cddr form-data)))
	(with-page (:title "Table Grinder")
	  (:header
	   (:h1 (string-upcase (getf form-data :table-name)))
	   (:h2 "Create Read Update Delete"))
	  (:section
	   (:nav  
	    (:menu
	     (:li (:a :href
		      (format nil "/table-view?name=~A" table-name)
		      (format nil "Table ~A" (string-upcase table-name))))))
	   (:article
	    (with-form table-name "/form-update" index row-plist)))
	  (with-footer)))
      (type-error () (hunchentoot:redirect "/cookie-error"))))

(hunchentoot:define-easy-handler (table-view :uri "/table-view") (name)
  (with-page (:title "Table Grinder")
    (:header
     (:h1 (string-upcase name))
     (:h2 "Create Read Update Delete"))
    (:section
     (:nav  
      (:menu
       (:li (:a :href "/database-tables" "Database Tables"))))
     (:article
      (with-table name "/form-view")))
     (with-footer)))

(hunchentoot:define-easy-handler (database-tables :uri "/database-tables") ()
  (with-page (:title "Table Grinder")
    (:header
     (:h1 "Database Tables"))
    (:section
     (:nav
      (:menu
       (:li (:a :href "/" "Home"))))
     (:article
      (:ul :class "table"
	   (dolist (row (pu:list-database-tables))
	     (:li (:a :href (format nil"table-view?name=~A" row) (string-upcase row)))))))
     (with-footer)))

(hunchentoot:define-easy-handler (post-login :uri "/post-login")(username password)
  :request-type :POST
  (let* ((database-name (getf (hunchentoot:session-value :database-info *session*) :database)))
    (start-db-connection :database database-name :database-user username :database-password password)
    (setf (hunchentoot:session-value :database-info) (list :database database-name :username username :password password))
    (hunchentoot:redirect "/")))

(hunchentoot:define-easy-handler (login :uri "/login") (database)
  (setf *session* (hunchentoot:start-session))
  (setf (hunchentoot:session-value :database-info *session*) (list :database database))
  (with-page (:title "Table Grinder")
    (:header
     (:h1 (format nil "Login to ~A" (string-upcase database))))
    (:section
     (:nav
      (:menu
       (:li (:a :href "/" "Home"))))
     (:article
      (:form :action "post-login" :method "post"
       (:table
	:class "container"
	(:tr
	 (:td (:label :for "username" "User name"))
	 (:td (:input :type "text" :name "username" :required t)))
	(:tr
	 (:td (:label :for "password" "Password"))
	 (:td (:input :type "password" :name "password" :required t))))
      (:div :class "container"
	    (:button :type "submit" "Login")))))
     (with-footer)))

(hunchentoot:define-easy-handler (root-web-page :uri "/") ()
  (with-page (:title "Table Grinder")
    (:header
     (:h1 "Table Grinder for Postmodern and Hunchentoot"))
    (:section
     (:nav
      (if *database*
	  (:menu
	   (unless (database-exists-p 'ibm_sample)
	     (make-sample-database))
	   (dolist (database (set-difference (list-databases) (list *database-user* "postgres") :test #'string=))
	     (:li (:a :href "/database-tables" (string-upcase database)))))
	  (:menu (:li (:a :href (format nil "/login?database=~A" *default-database*) "Login Database")))))
     (:article
      (:p "This is a proof of concept that you can grind a common lisp and web interface directly from a database table.")
      (:ul
       (:li "CLOS Class interface using the postmodern dau-class interface")
       (:li "HTML Table listing the records in a table")
       (:li "HTML Form for entering and editing table entries"))
      (:p "Basically CRUD without the cruft..")))
    (with-footer)))

(defun start-web-server ()
  "Start the web server. Reconnects if there is a unconnected webserver in *webserver-acceptor*.
Return boolean on wether *webserver-acceptor* is connected."
  (unless *webserver*
    (setf *webserver*
	  (hunchentoot:start
	   (make-instance
	       'hunchentoot:easy-acceptor
	     :port *webserver-port*)))
    (setf (hunchentoot:acceptor-document-root *webserver*)
	  (merge-pathnames (make-pathname :directory '(:relative "www"))
			   (asdf:system-source-directory "table-grinder")))
    (setf (hunchentoot:acceptor-error-template-directory *webserver*)
	  (merge-pathnames (make-pathname :directory '(:relative "www" "errors"))
			   (asdf:system-source-directory "table-grinder")))
    (setf hunchentoot:*dispatch-table*
		     (list 'dispatch-easy-handlers
			   (hunchentoot:create-folder-dispatcher-and-handler
			    "img/"
			    (merge-pathnames (make-pathname :directory '(:relative "www" "img"))
					     (asdf:system-source-directory "table-grinder"))))))
  (setf hunchentoot:*show-lisp-errors-p* t)
  *webserver*)

;;;----------------------------------------------------------------------
;;; Database

(defun start-db-connection (&key
			      (database *default-database*)
			      (database-user *database-user*)
			      (database-password *database-password*)
			      (host *database-host*))
  "Start the database connection. Reconnects if there is an unconnected database in *database* 
which matches the database parameter in the function, it will be reconnected. 
Returns boolean on whether the global *database* is now connected."
  (unless *database*
    (setf *database*
	  (connect database database-user database-password
			      host :pooled-p t))))

;;;----------------------------------------------------------------------
;;; Setup entry point

(defun make-sample-database ()
  (load "sample-database.lisp"))

  
(defun start-testing ()
  "Main entry point. Set up database connection create and fill mock database and start a web server"
  (unless *webserver*
    (if (start-web-server)
	(format t "Webserver started.")
	(error "Webserver failed to start.")))
    *webserver*)


(defun stop-testing ()
  "Basically tear down everything start-testing set up."
  (when *database*
    "(postmodern:clear-connection-pool)"
    (setf *database* nil)
    (format t "Database disconnected."))
  (when *webserver*
    (hunchentoot:stop *webserver*)
    (setf *webserver* nil)
    (format t "~%Webserver disconected."))
  (values))
