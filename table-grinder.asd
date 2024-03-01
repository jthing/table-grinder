
(defsystem "table-grinder"
  :version "0.0.1"
  :author "John Thingstad <jpthing@online.no>"
  :license "MIT"
  :depends-on (:cl-package-locks :hh-redblack :inferior-shell :alexandria
	       :postmodern :simple-date :simple-date/postgres-glue
	       :hunchentoot :spinneret :cl-css :parenscript)
  :components ((:module "src"
                :components
                ((:file "utils")
		 (:file "postmodern-utils" :depends-on ("utils"))
		 (:file "postmodern-web"   :depends-on ("utils"))
		 (:file "test-postmodern"  :depends-on ("postmodern-web" "postmodern-utils"))
		 (:file "main"             :depends-on ("test-postmodern")))))
  :description "Table Grinder for Postmodern and Hunchentoot 14/2-2024"
  :in-order-to ((test-op (test-op "table-grinder/tests"))))

(defsystem "table-grinder/tests"
  :author "John Thingstad"
  :license "MIT"
  :depends-on ("table-grinder"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for table-grinder"
  :perform (test-op (op c) (symbol-call :rove :run c)))
