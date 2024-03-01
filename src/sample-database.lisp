;;;; sample database - set up database with mock data
;;;; Based on a IBM SAMPLE database at https://www.ibm.com/docs/en/db2/11.5?topic=samples-sample-database
;;;; 10.02.2024 John Thingstad

(in-package :postmodern-web)

(unless (database-exists-p 'ibm_sample)
  (create-database
   "ibm_sample"
   :limit-public-access t
   :comment "Based on a IBM SAMPLE database at https://www.ibm.com/docs/en/db2/11.5?topic=samples-sample-database"))

(unless (string= (current-database) "ibm_sample")

  (start-db-connection :database "ibm_sample")

  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
	 simple-date-cl-postgres-glue:*simple-date-sql-readtable*)))

;;;
;;; ACT table
;;; 
;;;  Name:   	ACTNO   ACTKWD  ACTDESC
;;;  Type:	SMALLINT CHAR(6) VARCHAR(20)
;;; --------------------------------------------------
;;;  Values:	10	MANAGE	MANAGE/ADVISE
;;;   		20	ECOST	ESTIMATE COST
;;;   		30	DEFINE	DEFINE SPECS
;;;   		40	LEADPR	LEAD PROGRAM/DESIGN
;;;   		50	SPECS	WRITE SPECS
;;;   		60	LOGIC	DESCRIBE LOGIC
;;;   		70	CODE	CODE PROGRAMS
;;;   		80	TEST	TEST PROGRAMS
;;;   		90	ADMQS	ADM QUERY SYSTEM
;;;   		100	TEACH	TEACH CLASSES
;;;   		110	COURSE	DEVELOP COURSES
;;;   		120	STAFF	PERS AND STAFFING
;;;   		130	OPERAT	OPER COMPUTER SYS
;;;   		140	MAINT	MAINT SOFTWARE SYS
;;;   		150	ADMSYS	ADM OPERATING SYS
;;;   		160	ADMDB	ADM DATA BASES
;;;   		170	ADMDC	ADM DATA COMM
;;;   		180	DOC	DOCUMENT

(unless (table-exists-p 'act)
  (execute (:create-table
	    'act
	    ((actno :type smallint :primary-key t)
	     (actwd :type text)
	     (actdesc :type text))))

  (execute (:insert-rows-into 'act
	    :columns 'actno 'actwd 'actdesc
	    :values
	    '(( 10 "MANAGE" "MANAGE/ADVISE")
	      ( 20 "COST"   "ESTIMATE COST")
	      ( 30 "DEFINE" "DEFINE SPECS")
	      ( 40 "LEADPR" "LEAD PROGRAM/DESIGN")
	      ( 50 "SPECS"  "WRITE SPECS")
	      ( 60 "LOGIC"  "DESCRIBE LOGIC")
	      ( 70 "CODE"   "CODE PROGRAMS")
	      ( 80 "TEST"   "TEST PROGRAMS")
	      ( 90 "ADMQS"  "ADM EXECUTE SYSTEM")
	      (100 "TEACH"  "TEACH CLASSES")
	      (110 "COURSE" "DEVELOP COURSES")
	      (120 "STAFF"  "DEVELPERS AND STAFFING")
	      (130 "OPERAT" "OPER COMPUTER SYS")
	      (140 "MAINT"  "MAINT SOFTWARE SYS")
	      (150 "ADMSYS" "ADM OPERATING SYS")
	      (160 "ADMDB"  "ADM DATA BASES")
	      (170 "ADMDC"  "ADM DATA COMM")
	      (180 "DOC"    "DOCUMENT")))))


;;; ADEFUSR table
;;; Name:  WORKDEPT NO_OF_EMPLOYEES
;;; Type:	CHAR(3)	INTEGER
;;; Values:	A00	5
;;;  	B01	1
;;;  	C01	4
;;;  	D11	11
;;;  	D21	7
;;;  	E01	1
;;;  	E11	7
;;;  	E21	6

(unless (table-exists-p "adefusr")
  (execute (:create-table
	    'adefusr
	    ((workdept :type text)
	     (no_of_employees :type integer))))

  (execute (:insert-rows-into 'adefusr
	    :columns 'workdept 'no_of_employees
	    :values
	    '(("A00" 5)
	      ("B01" 1)
	      ("C01" 4)
	      ("D11" 11)
	      ("D21" 7)
	      ("E01" 1)
	      ("E11" 7)
	      ("E21" 6)))))

;;; CL_SCHED table
;;; Name:   CLASS_CODE        DAY STARTING  ENDING
;;; Type:	CHAR(7)	SMALLINT   TIME	        TIME
;;; Desc:	Class Code (room:teacher)	Day # of 4 day schedule	Class Start Time	Class End Time
;;; Values:	042:BF	4	12:10:00	14:00:00
;;;             553:MJA	1	10:30:00	11:00:00
;;;  	        543:CWM	3	09:10:00	10:30:00
;;;  	        778:RES	2	12:10:00	14:00:00
;;;  	        044:HD	3	17:12:30	18:00:00

(unless (table-exists-p "cl_shed")
  (execute (:create-table
	    'cl_shed
	    ((class_code :type text)
	     (day :type smallint)
	     (starting :type interval)
	     (ending :type interval))))

  (execute (:insert-rows-into 'cl_shed
	    :columns 'class_code 'day 'starting 'ending
	    :values
	    '(("042:BF"  4 "12:10:00" "14:00:00")
	      ("553:MJA" 1 "10:30:00" "11:00:00")
	      ("543:CWM" 3 "09:10:00" "10:30:00")
	      ("778:RES" 2 "12:10:00" "14:00:00")
	      ("044:HD"  3 "17:12:30" "18:00:00")))))

;; from this insane encoding ((((:MONTHS 0) (:DAYS 0) (:SECONDS 43800) (:USECONDS 0)))
;; produce this "12:10:00"
(defun interval->time (interval)
  (let* ((plist (flatten interval))
	 (seconds (getf plist :seconds))
	 (interval (multiple-value-list (simple-date:decode-interval (simple-date:encode-interval :second seconds)))))
    (format nil "~2,'0d:~2,'0d:~2,'0d" (nth 3 interval) (nth 4 interval) (nth 5 interval))))
;;; 
;;; DEPT table
;;; Name: DEPTNO DEPTNAME MGRNO ADMRDEPT LOCATION
;;;  CHAR(3)	VARCHAR(36)		        CHAR(6)	CHAR(3)	CHAR(16)
;;; 	A00	SPIFFY COMPUTER SERVICE DIV.	000010	A00	 
;;;  	B01	PLANNING			000020  A00	 
;;;  	C01	INFORMATION CENTER		000030	A00	 
;;;  	D01	DEVELOPMENT CENTER	 	        A00	 
;;;  	D11	MANUFACTURING SYSTEMS		000060	D01	 
;;;  	D21	ADMINISTRATION SYSTEMS		000070	D01	 
;;;  	E01	SUPPORT SERVICES		000050	A00	 
;;;  	E11	OPERATIONS			000090	E01	 
;;;  	E21	SOFTWARE SUPPORT		000100	E01	 
;;;  	F22	BRANCH OFFICE F2		 	E01	 
;;;  	G22	BRANCH OFFICE G2	 		E01	 
;;;  	H22	BRANCH OFFICE H2	 		E01	 
;;;  	I22	BRANCH OFFICE I2	 		E01	 
;;;  	J22	BRANCH OFFICE J2	 		E01	 
;;;

(unless (table-exists-p "dept")
  (execute (:create-table
	    'dept
	    ((deptno :type text)
	     (deptname :type text)
	     (mgrno :type (or text db-null))
	     (admrdept :type text)
	     (location :type (or text db-null)))))

  (execute (:insert-rows-into 'dept
	    :columns 'deptno 'deptname 'mgrno 'admrdept 'location
	    :values
	    '(("A00" "SPIFFY COMPUTER SERVICE DIV." "000010" "A00" :null)
	      ("B01" "PLANNING" "000020" "A00" :null)
	      ("C01" "INFORMATION CENTER" "000030" "A00" :null)
	      ("D01" "DEVELOPMENT CENTER" :null "A00" :null)
	      ("D11" "MANUFACTURING SYSTEMS" "000060" "D01" :null)
	      ("D21" "ADMINISTRATION SYSTEMS" "000070" "D01" :null)
	      ("E01" "SUPPORT SERVICES" "000050" "A00" :null)
	      ("E11" "OPERATIONS" "000090" "E01" :null)
	      ("E21" "SOFTWARE SUPPORT" "000100" "E01" :null)
	      ("F22" "BRANCH OFFICE F2" :null "E01" :null)
	      ("G22" "BRANCH OFFICE G2" :null "E01" :null)
	      ("H22" "BRANCH OFFICE H2" :null "E01" :null)
	      ("I22" "BRANCH OFFICE I2" :null "E01" :null)
	      ("J22" "BRANCH OFFICE J2" :null "E01" :null)))))

;;; 
;;; DEPARTMENT table
;;; Name: 	DEPTNO 	DEPTNAME 			MGRNO 	ADMRDEPT LOCATION
;;; Type:	CHAR(3)	 VARCHAR(29) 			CHAR(6)	CHAR(3)  CHAR(16)
;;;             NOT NULL NOT NULL                               NOT NULL
;;; 		A00	SPIFFY COMPUTER SERVICE DIV.	000010	A00	 
;;;  		B01	PLANNING			000020	A00	 
;;;  		C01	INFORMATION CENTER		000030	A00	 
;;;  		D01	DEVELOPMENT CENTER		 	A00	 
;;;  		D11	MANUFACTURING SYSTEMS		000060	D01	 
;;;  		D21	ADMINISTRATION SYSTEMS		000070	D01	 
;;;  		E01	SUPPORT SERVICES		000050	A00	 
;;;  		E11	OPERATIONS			000090	E01	 
;;;  		E21	SOFTWARE SUPPORT		000100	E01	 
;;;  		F22	BRANCH OFFICE F2		 	E01
;;;  		G22	BRANCH OFFICE G2		 	E01
;;;  		H22	BRANCH OFFICE H2		 	E01
;;;  		I22	BRANCH OFFICE I2		 	E01
;;;  

(unless (table-exists-p 'department)
  (execute (:create-table
	    'department
	    ((deptno :type text)
	     (deptname :type text)
	     (mgrno :type (or text db-null))
	     (admrdept :type text)
	     (location :type (or text db-null)))))

  (execute (:insert-rows-into 'department
	    :columns 'deptno 'deptname 'mgrno 'admrdept 'location
	    :values
	    '(("A00" "SPIFFY COMPUTER SERVICE DIV." "000010" "A00" :null)
	      ("B01" "PLANNING" "000020" "A00" :null)
	      ("C01" "INFORMATION CENTER" "000030" "A00" :null)	 
	      ("D01" "DEVELOPMENT CENTER" :null "A00" :null) 
	      ("D11" "MANUFACTURING SYSTEMS" "000060" "D01" :null)
	      ("D21" "ADMINISTRATION SYSTEMS" "000070" "D01" :null)
	      ("E01" "SUPPORT SERVICES" "000050" "A00" :null)
	      ("E11" "OPERATIONS" "000090" "E01" :null)
	      ("E21" "SOFTWARE SUPPORT" "000100" "E01" :null)
	      ("F22" "BRANCH OFFICE F2" :null "E01" :null)
	      ("G22" "BRANCH OFFICE G2" :null "E01" :null)
	      ("H22" "BRANCH OFFICE H2" :null "E01" :null)
	      ("I22" "BRANCH OFFICE I2" :null "E01" :null)
	      ("J22" "BRANCH OFFICE J2" :null "E01" :null)))))

;;; 
;;; EMPNO 	FIRSTNME MID INIT LASTNAME   WORK DEPT PHONE NO HIREDATE        JOB         ED LEVEL  SEX      BIRTHDATE       SALARY  BONUS   COMM
;;; CHAR(6) VARCHAR(12)CHAR(1)VARCHAR(15)	CHAR(3)	CHAR(4)	 DATE	        CHAR(8)     SMALLINT CHAR(1)    DATE          DECIMAL DECIMAL  DECIMAL
;;; NOT NULL    NOT NULL        NOT NULL                                                    NOT NULL                            (9,2)   (9,2)   (9,2)
;;; 000010	CHRISTINE   I  	HAAS		A00	3978	1965-01-01	PRES		18	F	1933-08-24	52750	1000	4220
;;; 000020	MICHAEL	    L	THOMPSON	B01	3476	1973-10-10	MANAGER		18	M	1948-02-02	41250	800	3300
;;; 000030	SALLY	    A	KWAN		C01	4738	1975-04-05	MANAGER		20	F	1941-05-11	38250	800	3060
;;; 000050	JOHN	    B	GEYER		E01	6789	1949-08-17	MANAGER		16	M	1925-09-15	40175	800	3214
;;; 000060	IRVING	    F	STERN		D11	6423	1973-09-14	MANAGER		16	M	1945-07-07	32250	500	2580
;;; 000070	EVA  	    D	PULASKI		D21	7831	1980-09-30	MANAGER		16	F	1953-05-26	36170	700	2893
;;; 000090	EILEEN	    W	HENDERSON	E11	5498	1970-08-15	MANAGER		16	F	1941-05-15	29750	600	2380
;;; 000100	THEODORE    Q 	SPENSER		E21	0972	1980-06-19	MANAGER		14	M	1956-12-18	26150	500	2092
;;; 000110	VINCENZO    G	LUCCHESSI	A00	3490	1958-05-16	SALESREP	19	M	1929-11-05	46500	900	3720
;;; 000120	SEAN	 	O'CONNELL	A00	2167	1963-12-05	CLERK		14	M	1942-10-18	29250	600	2340
;;; 000130	DOLORES	    M	QUINTANA	C01	4578	1971-07-28	ANALYST		16	F	1925-09-15	23800	500	1904
;;; 000140	HEATHER	    A	NICHOLLS	C01	1793	1976-12-15	ANALYST		18	F	1946-01-19	28420	600	2274
;;; 000150	BRUCE	 	ADAMSON		D11	4510	1972-02-12	DESIGNER	16	M	1947-05-17	25280	500	2022
;;; 000160	ELIZABETH   R 	PIANKA		D11	3782	1977-10-11	DESIGNER	17	F	1955-04-12	22250	400	1780
;;; 000170	MASATOSHI   J	YOSHIMURA	D11	2890	1978-09-15	DESIGNER	16	M	1951-01-05	24680	500	1974
;;; 000180	MARILYN	    S	SCOUTTEN	D11	1682	1973-07-07	DESIGNER	17	F	1949-02-21	21340	500	1707
;;; 000190	JAMES	    H	WALKER		D11	2986	1974-07-26	DESIGNER	16	M	1952-06-25	20450	400	1636
;;; 000200	DAVID	 	BROWN		D11	4501	1966-03-03	DESIGNER	16	M	1941-05-29	27740	600	2217
;;; 000210	WILLIAM	    T	JONES		D11	0942	1979-04-11	DESIGNER	17	M	1953-02-23	18270	400	1462
;;; 000220	JENNIFER    K	LUTZ		D11	0672	1968-08-29	DESIGNER	18	F	1948-03-19	29840	600	2387
;;; 000230	JAMES	    J	JEFFERSON	D21	2094	1966-11-21	CLERK		14	M	1935-05-30	22180	400	1774
;;; 000240	SALVATORE   M 	MARINO		D21	3780	1979-12-05	CLERK		17	M	1954-03-31	28760	600	2301
;;; 000250	DANIEL	    S	SMITH		D21	0961	1969-10-30	CLERK		15	M	1939-11-12	19180	400	1534
;;; 000260	SYBIL	    P	JOHNSON		D21	8953	1975-09-11	CLERK		16	F	1936-10-05	17250	300	1380
;;; 000270	MARIA	    L	PEREZ		D21	9001	1980-09-30	CLERK		15	F	1953-05-26	27380	500	2190
;;; 000280	ETHEL	    R	SCHNEIDER	E11	8997	1967-03-24	OPERATOR	17	F	1936-03-28	26250	500	2100
;;; 000290	JOHN	    R	PARKER		E11	4502	1980-05-30	OPERATOR	12	M	1946-07-09	15340	300	1227
;;; 000300	PHILIP	    X	SMITH		E11	2095	1972-06-19	OPERATOR	14	M	1936-10-27	17750	400	1420
;;; 000310	MAUDE	    F	SETRIGHT	E11	3332	1964-09-12	OPERATOR	12	F	1931-04-21	15900	300	1272
;;; 000320	RAMLAL	    V	MEHTA		E21	9990	1965-07-07	FIELDREP	16	M	1932-08-11	19950	400	1596
;;; 000330	WING	 	LEE		E21	2103	1976-02-23	FIELDREP	14	M	1941-07-18	25370	500	2030
;;; 000340	JASON	    R	GOUNOT		E21	5698	1947-05-05	FIELDREP	16	M	1926-05-17	23840	500	1907
;;; 

(unless (table-exists-p "employee")
  (execute (:create-table
	    'employee
	    ((empno :type text)
	     (firstname :type text)
	     (mid_init :type char)
	     (lastname :type text)
	     (work_dept :type text)
	     (phone_no :type text)
	     (hiredate :type date)
	     (job :type text)
	     (ed_level :type smallint)
	     (sex :type char)
	     (birthdate :type date)
	     (salary :type numeric)
	     (bonus :type numeric)
	     (comm :type decimal))))

  (execute (:insert-rows-into
	    'employee
	    :columns 'empno 'firstname 'mid_init 'lastname 'work_dept 'phone_no 'hiredate 'job 'ed_level 'sex 'birthdate 'salary 'bonus 'comm
	    :values
	    '(("000010" "CHRISTINE" "I" "HAAS" "A00" "3978" "1965-01-01" "PRES" 18 "F" "1933-08-24" 52750	1000 4220)
	      ("000020" "MICHAEL"	"L" "THOMPSON" "B01" "3476" "1973-10-10" "MANAGER" 18 "M" "1948-02-02" 41250 800 3300)
	      ("000030" "SALLY" "A" "KWAN" "C01" "4738" "1975-04-05" "MANAGER" 20	"F" "1941-05-11" 38250 800 3060)
	      ("000050" "JOHN" "B" "GEYER" "E01" "6789" "1949-08-17" "MANAGER" 16	"M" "1925-09-15" 40175 800 3214)
	      ("000060" "IRVING" "F" "STERN" "D11" "6423"	"1973-09-14" "MANAGER" 16 "M" "1945-07-07" 32250 500 2580)
	      ("000070" "EVA" "D"	"PULASKI" "D21"	"7831" "1980-09-30" "MANAGER" 16 "F" "1953-05-26" 36170	700 2893)
	      ("000090" "EILEEN" "W" "HENDERSON" "E11" "5498" "1970-08-15" "MANAGER" 16 "F" "1941-05-15" 29750 600 2380)
	      ("000100" "THEODORE" "Q" "SPENSER" "E21" "0972" "1980-06-19" "MANAGER" 14 "M" "1956-12-18" 26150 500 2092)
	      ("000110" "VINCENZO" "G" "LUCCHESSI" "A00" "3490" "1958-05-16" "SALESREP" 19 "M" "1929-11-05" 46500 900 3720)
	      ("000120" "SEAN" " " "O'CONNELL" "A00" "2167" "1963-12-05" "CLERK" 14 "M" "1942-10-18" 29250 600 2340)
	      ("000130" "DOLORES" "M" "QUINTANA" "C01" "4578" "1971-07-28" "ANALYST" 16 "F" "1925-09-15" 23800 500 1904)
	      ("000140" "HEATHER" "A" "NICHOLLS" "C01" "1793" "1976-12-15" "ANALYST" 18 "F" "1946-01-19" 28420 600 2274)
	      ("000150" "BRUCE" " " "ADAMSON" "D11" "4510" "1972-02-12" "DESIGNER" 16 "M" "1947-05-17" 25280 500 2022)
	      ("000160" "ELIZABETH" "R" "PIANKA" "D11" "3782" "1977-10-11" "DESIGNER" 17 "F" "1955-04-12"	22250 400 1780)
	      ("000170" "MASATOSHI" "J" "YOSHIMURA" "D11"	"2890" "1978-09-15" "DESIGNER" 16 "M" "1951-01-05" 24680 500 1974)
	      ("000180" "MARILYN" "S" "SCOUTTEN" "D11" "1682" "1973-07-07" "DESIGNER" 17 "F" "1949-02-21" 21340 500 1707)
	      ("000190" "JAMES" "H" "WALKER" "D11" "2986"	"1974-07-26" "DESIGNER"	16 "M" "1952-06-25" 20450 400 1636)
	      ("000200" "DAVID" " " "BROWN" "D11" "4501" "1966-03-03"	"DESIGNER" 16 "M" "1941-05-29" 27740 600 2217)
	      ("000210" "WILLIAM" "T" "JONES" "D11" "0942" "1979-04-11" "DESIGNER" 17 "M"	"1953-02-23" 18270 400 1462)
	      ("000220" "JENNIFER" "K" "LUTZ" "D11" "0672" "1968-08-29" "DESIGNER" 18 "F"	"1948-03-19" 29840 600 2387)
	      ("000230" "JAMES" "J" "JEFFERSON" "D21" "2094" "1966-11-21"	"CLERK" 14 "M"	"1935-05-30" 22180 400 1774)
	      ("000240" "SALVATORE" "M" "MARINO" "D21" "3780" "1979-12-05" "CLERK" 17 "M"	"1954-03-31" 28760 600 2301)
	      ("000250" "DANIEL" "S" "SMITH" "D21" "0961"	"1969-10-30" "CLERK" 15	"M" "1939-11-12" 19180	400 1534)
	      ("000260" "SYBIL" "P" "JOHNSON" "D21" "8953" "1975-09-11" "CLERK" 16 "F" "1936-10-05" 17250	300 1380)
	      ("000270" "MARIA" "L" "PEREZ" "D21"	"9001"	"1980-09-30" "CLERK" 15	"F" "1953-05-26" 27380 500 2190)
	      ("000280" "ETHEL" "R" "SCHNEIDER" "E11" "8997" "1967-03-24"	"OPERATOR" 17 "F" "1936-03-28" 26250 500 2100)
	      ("000290" "JOHN" "R" "PARKER" "E11"	"4502"	"1980-05-30" "OPERATOR"	12 "M" "1946-07-09" 15340 300 1227)
	      ("000300" "PHILIP" "X" "SMITH" "E11" "2095"	"1972-06-19" "OPERATOR"	14 "M" "1936-10-27" 17750 400 1420)
	      ("000310" "MAUDE" "F" "SETRIGHT" "E11" "3332" "1964-09-12" "OPERATOR" 12 "F" "1931-04-21" 15900 300	1272)
	      ("000320" "RAMLAL" "V" "MEHTA" "E21" "9990"	"1965-07-07" "FIELDREP"	16 "M"	"1932-08-11" 19950 400 1596)
	      ("000330" "WING" " " "LEE" "E21" "2103"	"1976-02-23" "FIELDREP"	14 "M"	"1941-07-18" 25370 500 2030)
	      ("000340" "JASON" "R" "GOUNOT" "E21" "5698"	"1947-05-05" "FIELDREP"	16 "M"	"1926-05-17" 23840 500 1907)))))

;;;
;;; Name:   EMPNO   PROJNO   ACTNO  EMPTIME EMSTDATE        EMENDATE
;;; Type:	CHAR(6) NOT NULL	CHAR(6) NOT NULL	SMALLINT NOT NULL	DEC(5,2)	DATE	DATE
;;; Desc:	Employee number	Project number	Activity number	Proportion of employee's time spent on project	Date activity starts	Date activity ends
;;; Values:	000010	AD3100	10	.50	1982-01-01	1982-07-01
;;;  	000070	AD3110	10	1.00	1982-01-01	1983-02-01
;;;  	000230	AD3111	60	1.00	1982-01-01	1982-03-15
;;;  	000230	AD3111	60	.50	1982-03-15	1982-04-15
;;;  	000230	AD3111	70	.50	1982-03-15	1982-10-15
;;;  	000230	AD3111	80	.50	1982-04-15	1982-10-15
;;;  	000230	AD3111	180	1.00	1982-10-15	1983-01-01
;;;  	000240	AD3111	70	1.00	1982-02-15	1982-09-15
;;;  	000240	AD3111	80	1.00	1982-09-15	1983-01-01
;;;  	000250	AD3112	60	1.00	1982-01-01	1982-02-01
;;;  	000250	AD3112	60	.50	1982-02-01	1982-03-15
;;;  	000250	AD3112	60	.50	1982-12-01	1983-01-01
;;;  	000250	AD3112	60	1.00	1983-01-01	1983-02-01
;;;  	000250	AD3112	70	.50	1982-02-01	1982-03-15
;;;  	000250	AD3112	70	1.00	1982-03-15	1982-08-15
;;;  	000250	AD3112	70	.25	1982-08-15	1982-10-15
;;;  	000250	AD3112	80	.25	1982-08-15	1982-10-15
;;;  	000250	AD3112	80	.50	1982-10-15	1982-12-01
;;;  	000250	AD3112	180	.50	1982-08-15	1983-01-01
;;;  	000260	AD3113	70	.50	1982-06-15	1982-07-01
;;;  	000260	AD3113	70	1.00	1982-07-01	1983-02-01
;;;  	000260	AD3113	80	1.00	1982-01-01	1982-03-01
;;;  	000260	AD3113	80	.50	1982-03-01	1982-04-15
;;;  	000260	AD3113	180	.50	1982-03-01	1982-04-15
;;;  	000260	AD3113	180	1.00	1982-04-15	1982-06-01
;;;  	000260	AD3113	180	.50	1982-06-01	1982-07-01
;;;  	000270	AD3113	60	.50	1982-03-01	1982-04-01
;;;  	000270	AD3113	60	1.00	1982-04-01	1982-09-01
;;;  	000270	AD3113	60	.25	1982-09-01	1982-10-15
;;;  	000270	AD3113	70	.75	1982-09-01	1982-10-15
;;;  	000270	AD3113	70	1.00	1982-10-15	1983-02-01
;;;  	000270	AD3113	80	1.00	1982-01-01	1982-03-01
;;;  	000270	AD3113	80	.50	1982-03-01	1982-04-01
;;;  	000030	IF1000	10	.50	1982-06-01	1983-01-01
;;;  	000130	IF1000	90	1.00	1982-01-01	1982-10-01
;;;  	000130	IF1000	100	.50	1982-10-01	1983-01-01
;;;  	000140	IF1000	90	.50	1982-10-01	1983-01-01
;;;  	000030	IF2000	10	.50	1982-01-01	1983-01-01
;;;  	000140	IF2000	100	1.00	1982-01-01	1982-03-01
;;;  	000140	IF2000	100	.50	1982-03-01	1982-07-01
;;;  	000140	IF2000	110	.50	1982-03-01	1982-07-01
;;;  	000140	IF2000	110	.50	1982-10-01	1983-01-01
;;;  	000010	MA2100	10	.50	1982-01-01	1982-11-01
;;;  	000110	MA2100	20	1.00	1982-01-01	1982-03-01
;;;  	000010	MA2110	10	1.00	1982-01-01	1983-02-01
;;;  	000200	MA2111	50	1.00	1982-01-01	1982-06-15
;;; 	000200	MA2111	60	1.00	1982-06-15	1983-02-01
;;;  	000220	MA2111	40	1.00	1982-01-01	1983-02-01
;;;  	000150	MA2112	60	1.00	1982-01-01	1982-07-15
;;;  	000150	MA2112	180	1.00	1982-07-15	1983-02-01
;;;  	000170	MA2112	60	1.00	1982-01-01	1983-06-01
;;;  	000170	MA2112	70	1.00	1982-06-01	1983-02-01
;;;  	000190	MA2112	70	1.00	1982-02-01	1982-10-01
;;;  	000190	MA2112	80	1.00	1982-10-01	1983-10-01
;;;  	000160	MA2113	60	1.00	1982-07-15	1983-02-01
;;;  	000170	MA2113	80	1.00	1982-01-01	1983-02-01
;;;  	000180	MA2113	70	1.00	1982-04-01	1982-06-15
;;;  	000210	MA2113	80	.50	1982-10-01	1983-02-01
;;;  	000210	MA2113	180	.50	1982-10-01	1983-02-01
;;;  	000050	OP1000	10	.25	1982-01-01	1983-02-01
;;;  	000090	OP1010	10	1.00	1982-01-01	1983-02-01
;;;  	000280	OP1010	130	1.00	1982-01-01	1983-02-01
;;;  	000290	OP1010	130	1.00	1982-01-01	1983-02-01
;;;  	000300	OP1010	130	1.00	1982-01-01	1983-02-01
;;;  	000310	OP1010	130	1.00	1982-01-01	1983-02-01
;;;  	000050	OP2010	10	.75	1982-01-01	1983-02-01
;;;  	000100	OP2010	10	1.00	1982-01-01	1983-02-01
;;;  	000320	OP2011	140	.75	1982-01-01	1983-02-01
;;;  	000320	OP2011	150	.25	1982-01-01	1983-02-01
;;;  	000330	OP2012	140	.25	1982-01-01	1983-02-01
;;;  	000330	OP2012	160	.75	1982-01-01	1983-02-01
;;;  	000340	OP2013	140	.50	1982-01-01	1983-02-01
;;;  	000340	OP2013	170	.50	1982-01-01	1983-02-01
;;;  	000020	PL2100	30	1.00	1982-01-01	1982-09-15
;;;

(unless (table-exists-p 'emp_act)
  (execute (:create-table
	    'emp_act
	    ((empno :type text)
	     (procno :type text)
	     (actno :type smallint)
	     (emptime :type numeric)
	     (emstdate :type date)
	     (emendate :type date))))

  (execute (:insert-rows-into
	    'emp-act
	    :columns 'empno 'procno 'actno 'emptime 'emstdate 'emendate
	    :values
	    '(("000010" "AD3100" 10 .50 "1982-01-01" "1982-07-01")
	      ("000070" "AD3110" 10 1.00 "1982-01-01" "1983-02-01")
	      ("000230" "AD3111" 60 1.00 "1982-01-01" "1982-03-15")
	      ("000230" "AD3111" 60 .50 "1982-03-15" "1982-04-15")
	      ("000230" "AD3111" 70 .50 "1982-03-15" "1982-10-15")
	      ("000230" "AD3111" 80 .50 "1982-04-15" "1982-10-15")
	      ("000230" "AD3111" 180 1.00 "1982-10-15" "1983-01-01")
	      ("000240" "AD3111" 70 1.00 "1982-02-15" "1982-09-15")
	      ("000240" "AD3111" 80 1.00 "1982-09-15" "1983-01-01")
	      ("000250" "AD3112" 60 1.00 "1982-01-01" "1982-02-01")
	      ("000250" "AD3112" 60 .50 "1982-02-01" "1982-03-15")
	      ("000250" "AD3112" 60 .50 "1982-12-01" "1983-01-01")
	      ("000250" "AD3112" 60 1.00 "1983-01-01" "1983-02-01")
	      ("000250" "AD3112" 70 .50 "1982-02-01" "1982-03-15")
	      ("000250" "AD3112" 70 1.00 "1982-03-15" "1982-08-15")
	      ("000250" "AD3112" 70 .25 "1982-08-15" "1982-10-15")
	      ("000250" "AD3112" 80 .25 "1982-08-15" "1982-10-15")
	      ("000250" "AD3112" 80 .50 "1982-10-15" "1982-12-01")
	      ("000250" "AD3112" 180 .50 "1982-08-15" "1983-01-01")
	      ("000260" "AD3113" 70 .50 "1982-06-15" "1982-07-01")
	      ("000260" "AD3113" 70 1.00 "1982-07-01" "1983-02-01")
	      ("000260" "AD3113" 80 1.00 "1982-01-01" "1982-03-01")
	      ("000260" "AD3113" 80 .50 "1982-03-01" "1982-04-15")
	      ("000260" "AD3113" 180 .50 "1982-03-01" "1982-04-15")
	      ("000260" "AD3113" 180 1.00 "1982-04-15" "1982-06-01")
	      ("000260" "AD3113" 180 .50 "1982-06-01" "1982-07-01")
	      ("000270" "AD3113" 60 .50 "1982-03-01" "1982-04-01")
	      ("000270" "AD3113" 60 1.00 "1982-04-01" "1982-09-01")
	      ("000270" "AD3113" 60 .25 "1982-09-01" "1982-10-15")
	      ("000270" "AD3113" 70 .75 "1982-09-01" "1982-10-15")
	      ("000270" "AD3113" 70 1.00 "1982-10-15" "1983-02-01")
	      ("000270" "AD3113" 80 1.00 "1982-01-01" "1982-03-01")
	      ("000270" "AD3113" 80 .50 "1982-03-01" "1982-04-01")
	      ("000030" "IF1000" 10 .50 "1982-06-01" "1983-01-01")
	      ("000130" "IF1000" 90 1.00 "1982-01-01" "1982-10-01")
	      ("000130" "IF1000" 100 .50 "1982-10-01" "1983-01-01")
	      ("000140" "IF1000" 90 .50 "1982-10-01" "1983-01-01")
	      ("000030" "IF2000" 10 .50 "1982-01-01" "1983-01-01")
	      ("000140" "IF2000" 100 1.00 "1982-01-01" "1982-03-01")
	      ("000140" "IF2000" 100 .50 "1982-03-01" "1982-07-01")
	      ("000140" "IF2000" 110 .50 "1982-03-01" "1982-07-01")
	      ("000140" "IF2000" 110 .50 "1982-10-01" "1983-01-01")
	      ("000010" "MA2100" 10 .50 "1982-01-01" "1982-11-01")
	      ("000110" "MA2100" 20 1.00 "1982-01-01" "1982-03-01")
	      ("000010" "MA2110" 10 1.00 "1982-01-01" "1983-02-01")
	      ("000200" "MA2111" 50 1.00 "1982-01-01" "1982-06-15")
	      ("000200" "MA2111" 60 1.00 "1982-06-15" "1983-02-01")
	      ("000220" "MA2111" 40 1.00 "1982-01-01" "1983-02-01")
	      ("000150" "MA2112" 60 1.00 "1982-01-01" "1982-07-15")
	      ("000150" "MA2112" 180 1.00 "1982-07-15" "1983-02-01")
	      ("000170" "MA2112" 60 1.00 "1982-01-01" "1983-06-01")
	      ("000170" "MA2112" 70 1.00 "1982-06-01" "1983-02-01")
	      ("000190" "MA2112" 70 1.00 "1982-02-01" "1982-10-01")
	      ("000190" "MA2112" 80 1.00 "1982-10-01" "1983-10-01")
	      ("000160" "MA2113" 60 1.00 "1982-07-15" "1983-02-01")
	      ("000170" "MA2113" 80 1.00 "1982-01-01" "1983-02-01")
	      ("000180" "MA2113" 70 1.00 "1982-04-01" "1982-06-15")
	      ("000210" "MA2113" 80 .50 "1982-10-01" "1983-02-01")
	      ("000210" "MA2113" 180 .50 "1982-10-01" "1983-02-01")
	      ("000050" "OP1000" 10 .25 "1982-01-01" "1983-02-01")
	      ("000090" "OP1010" 10 1.00 "1982-01-01" "1983-02-01")
	      ("000280" "OP1010" 130 1.00 "1982-01-01" "1983-02-01")
	      ("000290" "OP1010" 130 1.00 "1982-01-01" "1983-02-01")
	      ("000300" "OP1010" 130 1.00 "1982-01-01" "1983-02-01")
	      ("000310" "OP1010" 130 1.00 "1982-01-01" "1983-02-01")
	      ("000050" "OP2010" 10 .75 "1982-01-01" "1983-02-01")
	      ("000100" "OP2010" 10 1.00 "1982-01-01" "1983-02-01")
	      ("000320" "OP2011" 140 .75 "1982-01-01" "1983-02-01")
	      ("000320" "OP2011" 150 .25 "1982-01-01" "1983-02-01")
	      ("000330" "OP2012" 140 .25 "1982-01-01" "1983-02-01")
	      ("000330" "OP2012" 160 .75 "1982-01-01" "1983-02-01")
	      ("000340" "OP2013" 140 .50 "1982-01-01" "1983-02-01")
	      ("000340" "OP2013" 170 .50 "1982-01-01" "1983-02-01")
	      ("000020" "PL2100" 30 1.00 "1982-01-01" "1982-09-15")))))

;;; Type:	CHAR(6)         VARCHAR(10)    BLOB(100K)
;;;             NOT NULL        NOT NULL
;;; Desc:	Employee number	Photo format   Photo of employee
;;; Values:	000130		bitmap	       db200130.bmp
;;; 		000130		gif	       db200130.gif
;;; 		000140		bitmap	       db200140.bmp
;;;  		000140		gif	       db200140.gif
;;;	  	000150		bitmap	       db200150.bmp
;;;	  	000150		gif	       db200150.gif
;;;	  	000190		bitmap	       db200190.bmp
;;;	  	000190		gif	       db200190.gif

(unless (table-exists-p 'emp_photo)
  (execute (:create-table
	    'emp_photo
	    ((empno :type text)
	     (photo_format :type text)
	     (picture :type text))))

  (execute (:insert-rows-into
	    'emp_photo
	    :columns 'empno 'photo'format 'picture
	    :values
	    '(("000130" "bitmap" "db200130.bmp")
	      ("000130" "gif" "db200130.gif")
	      ("000140" "bitmap" "db200140.bmp")
	      ("000140" "gif" "db200140.gif")
	      ("000150" "bitmap" "db200150.bmp")
	      ("000150" "gif" "db200150.gif")
	      ("000190" "bitmap" "db200190.bmp")
	      ("000190" "gif" "db200190.gif")))))

;;; EMPPROJACT table
;;; Name:   	EMPNO   PROJNO  ACTNO   EMPTIME  EMSTDATE       EMENDATE
;;; Type:	CHAR(6)	CHAR(6)	SMALLINT DEC(5,2) DATE		DATE
;;; Values:	000070	AD3110	10	1.00	01/01/1982	02/01/1983
;;;	  	000230	AD3111	60	1.00	01/01/1982	03/15/1982
;;;	  	000230	AD3111	60	0.50	03/15/1982	04/15/1982
;;;	  	000230	AD3111	70	0.50	03/15/1982	10/15/1982
;;;	  	000230	AD3111	80	0.50	04/15/1982	10/15/1982
;;;	  	000230	AD3111	180	0.50	10/15/1982	01/01/1983
;;;	  	000240	AD3111	70	1.00	02/15/1982	09/15/1982
;;;	  	000240	AD3111	80	1.00	09/15/1982	01/01/1983
;;;	  	000250	AD3112	60	1.00	01/01/1982	02/01/1982
;;;	  	000250	AD3112	60	0.50	02/01/1982	03/15/1982
;;;	  	000250	AD3112	60	1.00	01/01/1983	02/01/1983
;;;	  	000250	AD3112	70	0.50	02/01/1982	03/15/1982
;;;	  	000250	AD3112	70	1.00	03/15/1982	08/15/1982
;;;	  	000250	AD3112	70	0.25	08/15/1982	10/15/1982
;;;	  	000250	AD3112	80	0.25	08/15/1982	10/15/1982
;;;	  	000250	AD3112	80	0.50	10/15/1982	12/01/1982
;;;	  	000250	AD3112	180	0.50	08/15/1982	01/01/1983
;;;	  	000260	AD3113	70	0.50	06/15/1982	07/01/1982
;;;	  	000260	AD3113	70	1.00	07/01/1982	02/01/1983
;;;	  	000260	AD3113	80	1.00	01/01/1982	03/01/1982
;;;	  	000260	AD3113	80	0.50	03/01/1982	04/15/1982
;;;	  	000260	AD3113	180	0.50	03/01/1982	04/15/1982
;;;	  	000260	AD3113	180	1.00	04/15/1982	06/01/1982
;;;	  	000260	AD3113	180	1.00	06/01/1982	07/01/1982
;;;	  	000270	AD3113	60	0.50	03/01/1982	04/01/1982
;;;	  	000270	AD3113	60	1.00	04/01/1982	09/01/1982
;;;	  	000270	AD3113	60	0.25	09/01/1982	10/15/1982
;;;	  	000270	AD3113	70	0.75	09/01/1982	10/15/1982
;;;	  	000270	AD3113	70	1.00	10/15/1982	02/01/1983
;;;	  	000270	AD3113	80	1.00	01/01/1982	03/01/1982
;;;	  	000270	AD3113	80	0.50	03/01/1982	04/01/1982
;;;	  	000030	IF1000	10	0.50	06/01/1982	01/01/1983
;;;	  	000130	IF1000	90	1.00	10/01/1982	01/01/1983
;;;	  	000130	IF1000	100	0.50	10/01/1982	01/01/1983
;;;	  	000140	IF1000	90	0.50	10/01/1982	01/01/1983
;;;	  	000030	IF2000	10	0.50	01/01/1982	01/01/1983
;;;	  	000140	IF2000	100	1.00	01/01/1982	03/01/1982
;;;	  	000140	IF2000	100	0.50	03/01/1982	07/01/1982
;;;  		000140	IF2000	110	0.50	03/01/1982	07/01/1982
;;;  		000140	IF2000	110	0.50	10/01/1982	01/01/1983
;;;  		000010	MA2100	10	0.50	01/01/1982	11/01/1982
;;;  		000110	MA2100	20	1.00	01/01/1982	03/01/1983
;;;  		000010	MA2110	10	1.00	01/01/1982	02/01/1983
;;;  		000200	MA2111	50	1.00	01/01/1982	06/15/1982
;;;  		000200	MA2111	60	1.00	06/15/1982	02/01/1983
;;;  		000220	MA2111	40	1.00	01/01/1982	02/01/1983
;;;  		000150	MA2112	60	1.00	01/01/1982	07/15/1982
;;;  		000150	MA2112	180	1.00	07/15/1982	02/01/1983
;;;  		000170	MA2112	60	1.00	01/01/1982	06/01/1983
;;;  		000170	MA2112	70	1.00	06/01/1982	02/01/1983
;;;  		000190	MA2112	70	1.00	01/01/1982	10/01/1982
;;;  		000190	MA2112	80	1.00	10/01/1982	10/01/1982
;;;  		000160	MA2113	60	1.00	07/15/1982	02/01/1983
;;;  		000170	MA2113	80	1.00	01/01/1982	02/01/1983
;;;  		000180	MA2113	70	1.00	04/01/1982	06/15/1982
;;;  		000210	MA2113	80	0.50	10/01/1982	02/01/1983
;;;  		000210	MA2113	180	0.50	10/01/1982	02/01/1983
;;;  		000050	OP1000	10	0.25	01/01/1982	02/01/1983
;;;  		000090	OP1010	10	1.00	01/01/1982	02/01/1983
;;;  		000280	OP1010	130	1.00	01/01/1982	02/01/1983
;;;  		000290	OP1010	130	1.00	01/01/1982	02/01/1983
;;;  		000300	OP1010	130	1.00	01/01/1982	02/01/1983
;;;  		000310	OP1010	130	1.00	01/01/1982	02/01/1983
;;;  		000050	OP1010	10	0.75	01/01/1982	02/01/1983
;;;  		000100	OP1010	10	1.00	01/01/1982	02/01/1983
;;;  		000320	OP2011	140	0.75	01/01/1982	02/01/1983
;;;  		000320	OP2011	150	0.25	01/01/1982	02/01/1983
;;;  		000330	OP2012	140	0.25	01/01/1982	02/01/1983
;;;  		000330	OP2012	160	0.75	01/01/1982	02/01/1983
;;;  		000340	OP2013	140	0.50	01/01/1982	02/01/1983
;;;  		000340	OP2013	170	0.50	01/01/1982	02/01/1983
;;;  		000020	PL2100	30	1.00	01/01/1982	09/15/1982


;; NB! Yes, just a copy of emp_act ... almost

(unless (table-exists-p 'empprojact)
  (execute (:create-table
	    'empprojact
	    ((empno :type text)
	     (procno :type text)
	     (actno :type smallint)
	     (emptime :type numeric)
	     (emstdate :type date)
	     (emendate :type date))))

  (execute (:insert-rows-into
	    'empprojact
	    :columns 'empno 'procno 'actno 'emptime 'emstdate 'emendate
	    :values
	    '(("000070"	"AD3110"	10	1.00	"01/01/1982"	"02/01/1983")
	      ("000230"	"AD3111"	60	1.00	"01/01/1982"	"03/15/1982")
	      ("000230"	"AD3111"	60	0.50	"03/15/1982"	"04/15/1982")
	      ("000230"	"AD3111"	70	0.50	"03/15/1982"	"10/15/1982")
	      ("000230"	"AD3111"	80	0.50	"04/15/1982"	"10/15/1982")
	      ("000230"	"AD3111"	180	0.50	"10/15/1982"	"01/01/1983")
	      ("000240"	"AD3111"	70	1.00	"02/15/1982"	"09/15/1982")
	      ("000240"	"AD3111"	80	1.00	"09/15/1982"	"01/01/1983")
	      ("000250"	"AD3112"	60	1.00	"01/01/1982"	"02/01/1982")
	      ("000250"	"AD3112"	60	0.50	"02/01/1982"	"03/15/1982")
	      ("000250"	"AD3112"	60	1.00	"01/01/1983"	"02/01/1983")
	      ("000250"	"AD3112"	70	0.50	"02/01/1982"	"03/15/1982")
	      ("000250"	"AD3112"	70	1.00	"03/15/1982"	"08/15/1982")
	      ("000250"	"AD3112"	70	0.25	"08/15/1982"	"10/15/1982")
	      ("000250"	"AD3112"	80	0.25	"08/15/1982"	"10/15/1982")
	      ("000250"	"AD3112"	80	0.50	"10/15/1982"	"12/01/1982")
	      ("000250"	"AD3112"	180	0.50	"08/15/1982"	"01/01/1983")
	      ("000260"	"AD3113"	70	0.50	"06/15/1982"	"07/01/1982")
	      ("000260"	"AD3113"	70	1.00	"07/01/1982"	"02/01/1983")
	      ("000260"	"AD3113"	80	1.00	"01/01/1982"	"03/01/1982")
	      ("000260"	"AD3113"	80	0.50	"03/01/1982"	"04/15/1982")
	      ("000260"	"AD3113"	180	0.50	"03/01/1982"	"04/15/1982")
	      ("000260"	"AD3113"	180	1.00	"04/15/1982"	"06/01/1982")
	      ("000260"	"AD3113"	180	1.00	"06/01/1982"	"07/01/1982")
	      ("000270"	"AD3113"	60	0.50	"03/01/1982"	"04/01/1982")
	      ("000270"	"AD3113"	60	1.00	"04/01/1982"	"09/01/1982")
	      ("000270"	"AD3113"	60	0.25	"09/01/1982"	"10/15/1982")
	      ("000270"	"AD3113"	70	0.75	"09/01/1982"	"10/15/1982")
	      ("000270"	"AD3113"	70	1.00	"10/15/1982"	"02/01/1983")
	      ("000270"	"AD3113"	80	1.00	"01/01/1982"	"03/01/1982")
	      ("000270"	"AD3113"	80	0.50	"03/01/1982"	"04/01/1982")
	      ("000030"	"IF1000"	10	0.50	"06/01/1982"	"01/01/1983")
	      ("000130"	"IF1000"	90	1.00	"10/01/1982"	"01/01/1983")
	      ("000130"	"IF1000"	100	0.50	"10/01/1982"	"01/01/1983")
	      ("000140"	"IF1000"	90	0.50	"10/01/1982"	"01/01/1983")
	      ("000030"	"IF2000"	10	0.50	"01/01/1982"	"01/01/1983")
	      ("000140"	"IF2000"	100	1.00	"01/01/1982"	"03/01/1982")
	      ("000140"	"IF2000"	100	0.50	"03/01/1982"	"07/01/1982")
	      ("000140"	"IF2000"	110	0.50	"03/01/1982"	"07/01/1982")
	      ("000140"	"IF2000"	110	0.50	"10/01/1982"	"01/01/1983")
	      ("000010"	"MA2100"	10	0.50	"01/01/1982"	"11/01/1982")
	      ("000110"	"MA2100"	20	1.00	"01/01/1982"	"03/01/1983")
	      ("000010"	"MA2110"	10	1.00	"01/01/1982"	"02/01/1983")
	      ("000200"	"MA2111"	50	1.00	"01/01/1982"	"06/15/1982")
	      ("000200"	"MA2111"	60	1.00	"06/15/1982"	"02/01/1983")
	      ("000220"	"MA2111"	40	1.00	"01/01/1982"	"02/01/1983")
	      ("000150"	"MA2112"	60	1.00	"01/01/1982"	"07/15/1982")
	      ("000150"	"MA2112"	180	1.00	"07/15/1982"	"02/01/1983")
	      ("000170"	"MA2112"	60	1.00	"01/01/1982"	"06/01/1983")
	      ("000170"	"MA2112"	70	1.00	"06/01/1982"	"02/01/1983")
	      ("000190"	"MA2112"	70	1.00	"01/01/1982"	"10/01/1982")
	      ("000190"	"MA2112"	80	1.00	"10/01/1982"	"10/01/1982")
	      ("000160"	"MA2113"	60	1.00	"07/15/1982"	"02/01/1983")
	      ("000170"	"MA2113"	80	1.00	"01/01/1982"	"02/01/1983")
	      ("000180"	"MA2113"	70	1.00	"04/01/1982"	"06/15/1982")
	      ("000210"	"MA2113"	80	0.50	"10/01/1982"	"02/01/1983")
	      ("000210"	"MA2113"	180	0.50	"10/01/1982"	"02/01/1983")
	      ("000050"	"OP1000"	10	0.25	"01/01/1982"	"02/01/1983")
	      ("000090"	"OP1010"	10	1.00	"01/01/1982"	"02/01/1983")
	      ("000280"	"OP1010"	130	1.00	"01/01/1982"	"02/01/1983")
	      ("000290"	"OP1010"	130	1.00	"01/01/1982"	"02/01/1983")
	      ("000300"	"OP1010"	130	1.00	"01/01/1982"	"02/01/1983")
	      ("000310"	"OP1010"	130	1.00	"01/01/1982"	"02/01/1983")
	      ("000050"	"OP1010"	10	0.75	"01/01/1982"	"02/01/1983")
	      ("000100"	"OP1010"	10	1.00	"01/01/1982"	"02/01/1983")
	      ("000320"	"OP2011"	140	0.75	"01/01/1982"	"02/01/1983")
	      ("000320"	"OP2011"	150	0.25	"01/01/1982"	"02/01/1983")
	      ("000330"	"OP2012"	140	0.25	"01/01/1982"	"02/01/1983")
	      ("000330"	"OP2012"	160	0.75	"01/01/1982"	"02/01/1983")
	      ("000340"	"OP2013"	140	0.50	"01/01/1982"	"02/01/1983")
	      ("000340"	"OP2013"	170	0.50	"01/01/1982"	"02/01/1983")
	      ("000020"	"PL2100"	30	1.00	"01/01/1982"	"09/15/1982")))))

;;; EMP_RESUME table
;;; Name: 	EMPNO 		RESUME_FORMAT 	RESUME
;;; Type:	CHAR(6)         VARCHAR(10) 	CLOB(5K)
;;;		NOT NULL	NOT NULL
;;; Desc:	Employee number	Resume Format	Resume of employee
;;; Values:	000130		ascii		db200130.asc
;;; 	 	000130		html		db200130.htm
;;; 	 	000140		ascii		db200140.asc
;;; 	 	000140		html		db200140.htm
;;; 	 	000150		ascii		db200150.asc
;;; 	 	000150		html		db200150.htm
;;; 	 	000190		ascii		db200190.asc
;;; 	 	000190		html		db200190.htm

(unless (table-exists-p 'emp_resume)
  (execute (:create-table
	    'emp_resume
	    ((empno :type text)
	     (resume_format :type text)
	     (resume :type text))))

  (execute (:insert-rows-into
	    'emp_resume
	    :columns 'empno 'resume_format 'resume
	    :values
	    '(("000130" "ascii" "db200130.asc")
	      ("000130" "html" "db200130.htm")
	      ("000140" "ascii" "db200140.asc")
	      ("000140" "html" "db200140.htm")
	      ("000150" "ascii" "db200150.asc")
	      ("000150" "html" "db200150.htm")
	      ("000190" "ascii" "db200190.asc")
	      ("000190" "html" "db200190.htm")))))

;;; Name: 	RECEIVED 	SOURCE 	SUBJECT 	NOTE_TEXT
;;; Type:	TIMESTAMP	CHAR(8)	CHAR(64)	VARCHAR(3000)
;;; Desc:	Date and Time received	User id of person sending note	Brief description	The note


(unless (table-exists-p 'in_tray)
  (execute (:create-table
	    'in_tray
	    ((recieved :type (or timestamp db-null))
	     (source :type (or text db-null))
	     (subject :type (or text db-null))
	     (note_text :type text))))

  (execute (:insert-rows-into
	    'in_tray
	    :columns 'recieved 'source 'subject 'note_text
	    :values
	    '((:null :null :null
	       "To: JWALKER Cc: QUINTANA, NICHOLLS Jim, Looks like our hard work has paid off. I have some good beer in the fridge if you want to come over to celebrate a bit. Delores and Heather, are you interested as well? Bruce <Forwarding from ISTERN> Subject: FWD: Fantastic year! 4th Quarter Bonus. To: Dept_D11")

	      ("1988-12-25 17:12:30.0" "BADAMSON" "FWD: Fantastic year! 4th Quarter Bonus."
	       "Congratulations on a job well done. Enjoy this year's bonus. Irv <Forwarding from CHAAS> Subject: Fantastic year! 4th Quarter Bonus. To: All_Managers Our 4th quarter results are in. We pulled together as a team and exceeded our plan! I am pleased to announce a bonus this year of 18%. Enjoy the holidays. Christine Haas")

	      ("1988-12-23 08:53:58.0" "ISTERN" "FWD: Fantastic year! 4th Quarter Bonus."
	       "To: Dept_D11 Congratulations on a job well done. Enjoy this year's bonus. Irv <Forwarding from CHAAS> Subject: Fantastic year! 4th Quarter Bonus. To: All_Managers Our 4th quarter results are in. We pulled together as a team and exceeded our plan! I am pleased to announce a bonus this year of 18%. Enjoy the holidays. Christine Haas")

	      ("1988-12-22 14:07:21.1" "CHAAS" "Fantastic year! 4th Quarter Bonus."
	       "To: All_Managers Our 4th quarter results are in. We pulled together as a team and exceeded our plan! I am pleased to announce a bonus this year of 18%. Enjoy the holidays. Christine Haas")))))

;;; ORG table
;;; Desc:	Department number, Department name, Manager number, Division of corporation, City
;;; Name: 	DEPTNUMB DEPTNAME 	MANAGER  DIVISION 	LOCATION
;;; Type:	SMALLINT VARCHAR(14)	SMALLINT VARCHAR(10)	VARCHAR(13)
;;;	 	NOT NULL
;;; Values:	10	Head Office	160	Corporate	New York
;;;  		15	New England	50	Eastern		Boston
;;;  		20	Mid Atlantic	10	Eastern		Washington
;;;  		38	South Atlantic	30	Eastern		Atlanta
;;;  		42	Great Lakes	100	Midwest		Chicago
;;;  		51	Plains		140	Midwest		Dallas
;;;  		66	Pacific		270	Western		San Francisco
;;;  		84	Mountain	290	Western		Denver

(unless (table-exists-p 'org)
  (execute (:create-table
	    'org
	    ((deptnumb :type smallint)
	     (deptname :type text)
	     (manager :type smallint)
	     (division :type text)
	     (location :type text))))

  (execute (:insert-rows-into
	    'org
	    :columns 'deptnumb 'deptname 'manager 'division 'location
	    :values
	    '((10 "Head Office" 160 "Corporate" "New York")
	      (15 "New England" 50 "Eastern" "Boston")
	      (20 "Mid Atlantic" 10 "Eastern" "Washington")
	      (38 "South Atlantic" 30 "Eastern" "Atlanta")
	      (42 "Great Lakes" 100 "Midwest" "Chicago")
	      (51 "Plains" 140 "Midwest" "Dallas")
	      (66 "Pacific" 270 "Western" "San Francisco")
	      (84 "Mountain" 290 "Western" "Denver")))))

;;; PROJ table
;;; Name:   PROJNO 	PROJNAME 		DEPTNO 	RESPEMP PRSTAFF PRSTDATE 	PRENDATE 	MAJPROJ
;;; Type:	CHAR(6)	VARCHAR(36)		CHAR(3)	CHAR(6)	DEC(5,2) DATE		DATE		CHAR(6)
;;; Values:	AD3100	ADMIN SERVICES		D01	000010	6.50	01/01/1982	02/01/1983	 
;;; 	 	AD3110	GENERAL ADMIN SYSTEMS	D21	000070	6.00	01/01/1982	02/01/1983	AD3100
;;; 	 	AD3111	PAYROLL PROGRAMMING	D21	000230	2.00	01/01/1982	02/01/1983	AD3100
;;; 	 	AD3112	PERSONNEL PROGRAMMING	D21	000250	1.00	01/01/1982	02/01/1983	AD3100
;;; 	 	AD3113	ACCOUNT PROGRAMMING	D21	000270	2.00	01/01/1982	02/01/1983	AD3100
;;; 	 	IF1000	EXECUTE SERVICES		C01	000030	2.00	01/01/1982	02/01/1983	 
;;; 	 	IF2000	USER EDUCATION		C01	000030	1.00	01/01/1982	02/01/1983	 
;;; 	 	MA2100	WELD LINE AUTOMATION	D01	000010	12.00	01/01/1982	02/01/1983	 
;;; 	 	MA2110	W L PROGRAMMING		D11	000060	9.00	01/01/1982	02/01/1983	MA2100
;;; 	 	MA2111	W L PROGRAM DESIGN	D11	000220	2.00	01/01/1982	12/01/1982	MA2100
;;; 	 	MA2112	W L ROBOT DESIGN	D11	000150	3.00	01/01/1982	12/01/1982	MA2100
;;; 	 	MA2113	W L PROD CONT PROGS	D11	000160	3.00	02/15/1982	12/01/1982	MA2100
;;; 	 	OP1000	OPERATION SUPPORT	E01	000050	6.00	01/01/1982	02/01/1983	 
;;; 	 	OP1010	OPERATION		E11	000090	5.00	01/01/1982	02/01/1983	OP1000
;;; 	 	OP2000	GEN SYSTEMS SERVICES	E01	000050	5.00	01/01/1982	02/01/1983	 
;;; 	 	OP2010	SYSTEMS SUPPORT		E21	000100	4.00	01/01/1982	02/01/1983	OP2010
;;; 	 	OP2011	SCP SYSTEMS SUPPORT	E21	000320	1.00	01/01/1982	02/01/1983	OP2010
;;; 	 	OP2012	APPLICATIONS SUPPORT	E21	000330	1.00	01/01/1982	02/01/1983	OP2010
;;; 	 	OP2013	DB/DC SUPPORT		E21	000340	1.00	01/01/1982	02/01/1983	OP2010
;;; 	 	PL2100	WELD LINE PLANNING	B01	000020	1.00	01/01/1982	09/15/1982	MA2100

(unless (table-exists-p 'proj)
  (execute (:create-table '
	    proj
	    ((projno   :type text)
	     (projname :type text)
	     (deptno   :type text)
	     (respemp  :type text)
	     (prstaff  :type numeric)
	     (prstdate :type date)
	     (prendate :type date)
	     (majproj  :type (or text db-null)))))

  (execute (:insert-rows-into
	    'proj
	    :columns 'projno 'projname 'deptno 'respemp  'prstaff 'prstdate 'prendate 'majproj
	    :values
	    '(("AD3100" "ADMIN SERVICES" "D01" "000010" 6.50 "01/01/1982" "02/01/1983" :null) 
	      ("AD3110" "GENERAL ADMIN SYSTEMS" "D21" "000070" 6.00 "01/01/1982" "02/01/1983" "AD3100")
	      ("AD3111" "PAYROLL PROGRAMMING" "D21" "000230" 2.00 "01/01/1982" "02/01/1983" "AD3100")
	      ("AD3112" "PERSONNEL PROGRAMMING" "D21" "000250" 1.00 "01/01/1982" "02/01/1983" "AD3100")
	      ("AD3113" "ACCOUNT PROGRAMMING" "D21" "000270" 2.00 "01/01/1982" "02/01/1983" "AD3100")
	      ("IF1000" "EXECUTE SERVICES" "C01" "000030" 2.00 "01/01/1982" "02/01/1983" :null) 
	      ("IF2000" "USER EDUCATION" "C01" "000030" 1.00 "01/01/1982" "02/01/1983" :null) 
	      ("MA2100" "WELD LINE AUTOMATION" "D01" "000010" 12.00 "01/01/1982" "02/01/1983" :null) 
	      ("MA2110" "W L PROGRAMMING" "D11" "000060" 9.00 "01/01/1982" "02/01/1983" "MA2100")
	      ("MA2111" "W L PROGRAM DESIGN" "D11" "000220" 2.00 "01/01/1982" "12/01/1982" "MA2100")
	      ("MA2112" "W L ROBOT DESIGN" "D11" "000150" 3.00 "01/01/1982" "12/01/1982" "MA2100")
	      ("MA2113" "W L PROD CONT PROGS" "D11" "000160" 3.00 "02/15/1982" "12/01/1982" "MA2100")
	      ("OP1000" "OPERATION SUPPORT" "E01" "000050" 6.00 "01/01/1982" "02/01/1983" :null) 
	      ("OP1010" "OPERATION" "E11" "000090" 5.00 "01/01/1982" "02/01/1983" "OP1000")
	      ("OP2000" "GEN SYSTEMS SERVICES" "E01" "000050" 5.00 "01/01/1982" "02/01/1983" :null) 
	      ("OP2010" "SYSTEMS SUPPORT" "E21" "000100" 4.00 "01/01/1982" "02/01/1983" "OP2010")
	      ("OP2011" "SCP SYSTEMS SUPPORT" "E21" "000320" 1.00 "01/01/1982" "02/01/1983" "OP2010")
	      ("OP2012" "APPLICATIONS SUPPORT" "E21" "000330" 1.00 "01/01/1982" "02/01/1983" "OP2010")
	      ("OP2013" "DB/DC SUPPORT" "E21" "000340" 1.00 "01/01/1982" "02/01/1983" "OP2010")
	      ("PL2100" "WELD LINE PLANNING" "B01" "000020" 1.00 "01/01/1982" "09/15/1982" "MA2100")))))

;;; PROJACT table
;;; Name: 	PROJNO 	ACTNO 	   ACSTAFF 	ACSTDATE 	ACENDATE
;;; Type:	CHAR(6)	SMALLINT   DEC(5,2)	DATE		DATE
;;; Values:	AD3100	10	 		01/01/1982	 
;;; 	 	AD3110	10	 		01/01/1982	 
;;; 	 	AD3111	60	 		01/01/1982	 
;;; 	 	AD3111	60	 		03/15/1982	 
;;; 	 	AD3111	70	 		03/15/1982	 
;;; 	 	AD3111	80	 		04/15/1982	 
;;; 	 	AD3111	180	 		10/15/1982	 
;;; 	 	AD3111	70	 		02/15/1982	 
;;; 	 	AD3111	80	 		09/15/1982	 
;;; 	 	AD3112	60	 		01/01/1982	 
;;; 	 	AD3112	60	 		02/01/1982	 
;;; 	 	AD3112	60	 		01/01/1983	 
;;; 	 	AD3112	70	 		02/01/1982	 
;;; 	 	AD3112	70	 		03/15/1982	 
;;; 	 	AD3112	70	 		08/15/1982	 
;;; 	 	AD3112	80	 		08/15/1982	 
;;; 	 	AD3112	80	 		10/15/1982	 
;;; 	 	AD3112	180	 		08/15/1982	 
;;; 	 	AD3113	70	 		06/15/1982	 
;;; 	 	AD3113	70	 		07/01/1982	 
;;; 	 	AD3113	80	 		01/01/1982	 
;;; 	 	AD3113	80	 		03/01/1982	 
;;; 	 	AD3113	180	 		03/01/1982	 
;;; 	 	AD3113	180	 		04/15/1982	 
;;; 	 	AD3113	180	 		06/01/1982	 
;;; 	 	AD3113	60	 		03/01/1982	 
;;; 	 	AD3113	60	 		04/01/1982	 
;;; 	 	AD3113	60	 		09/01/1982	 
;;; 	 	AD3113	70	 		09/01/1982	 
;;; 	 	AD3113	70	 		10/15/1982	 
;;; 	 	IF1000	10	 		06/01/1982	 
;;; 	 	IF1000	90	 		10/01/1982	 
;;; 	 	IF1000	100	 		10/01/1982	 
;;; 	 	IF2000	10	 		01/01/1982	 
;;; 	 	IF2000	100	 		01/01/1982	 
;;; 	 	IF2000	100	 		03/01/1982	 
;;; 	 	IF2000	110	 		03/01/1982	 
;;; 	 	IF2000	110	 		10/01/1982	 
;;; 	 	MA2100	10	 		01/01/1982	 
;;; 	 	MA2100	20	 		01/01/1982	 
;;; 	 	MA2110	10	 		01/01/1982	 
;;; 	 	MA2111	50	 		01/01/1982	 
;;; 	 	MA2111	60	 		06/15/1982	 
;;; 	 	MA2111	40	 		01/01/1982	 
;;; 	 	MA2112	60	 		01/01/1982	 
;;; 	 	MA2112	180	 		07/15/1982	 
;;; 	 	MA2112	70	 		06/01/1982	 
;;; 	 	MA2112	70	 		01/01/1982	 
;;;	 	MA2112	80	 		10/01/1982	 
;;;	 	MA2113	60	 		07/15/1982	 
;;;	 	MA2113	80	 		01/01/1982	 
;;;	 	MA2113	70	 		04/01/1982	 
;;;	 	MA2113	80	 		10/01/1982	 
;;;	 	MA2113	180	 		10/01/1982	 
;;;	 	OP1000	10	 		01/01/1982	 
;;;	 	OP1010	10	 		01/01/1982	 
;;;	 	OP1010	130	 		01/01/1982	 
;;;	 	OP2010	10	 		01/01/1982	 
;;;	 	OP2011	140	 		01/01/1982	 
;;;	 	OP2011	150	 		01/01/1982	 
;;;	 	OP2012	140	 		01/01/1982	 
;;;	 	OP2012	160	 		01/01/1982	 
;;;	 	OP2013	140	 		01/01/1982	 
;;;	 	OP2013	170	 		01/01/1982	 
;;;	 	PL2100	30	 		01/01/1982	 

(unless (table-exists-p 'projact)
  (execute (:create-table 
	    'projact
	    ((projno   :type text)
	     (actno    :type smallint)
	     (acstaff :type (or numeric db-null))
	     (acstdate :type date)
	     (acendate :type (or date db-null)))))

  (execute (:insert-rows-into
	    'projact
	    :columns 'projno 'actno 'acstaff 'acstdate 'acendate
	    :values
	    '(("AD3100" 10 :null "01/01/1982" :null)
	      ("AD3110" 10 :null "01/01/1982" :null)
	      ("AD3111" 60 :null "01/01/1982" :null)
	      ("AD3111" 60 :null "03/15/1982" :null)
	      ("AD3111" 70 :null "03/15/1982" :null)
	      ("AD3111" 80 :null "04/15/1982" :null)
	      ("AD3111" 180 :null "10/15/1982" :null)
	      ("AD3111" 70 :null "02/15/1982" :null)
	      ("AD3111" 80 :null "09/15/1982" :null)
	      ("AD3112" 60 :null "01/01/1982" :null)
	      ("AD3112" 60 :null "02/01/1982" :null)
	      ("AD3112" 60 :null "01/01/1983" :null)
	      ("AD3112" 70 :null "02/01/1982" :null)
	      ("AD3112" 70 :null "03/15/1982" :null)
	      ("AD3112" 70 :null "08/15/1982" :null)
	      ("AD3112" 80 :null "08/15/1982" :null)
	      ("AD3112" 80 :null "10/15/1982" :null)
	      ("AD3112" 180 :null "08/15/1982" :null)
	      ("AD3113" 70 :null "06/15/1982" :null)
	      ("AD3113" 70 :null "07/01/1982" :null)
	      ("AD3113" 80 :null "01/01/1982" :null)
	      ("AD3113" 80 :null "03/01/1982" :null)
	      ("AD3113" 180 :null "03/01/1982" :null)
	      ("AD3113" 180 :null "04/15/1982" :null)
	      ("AD3113" 180 :null "06/01/1982" :null)
	      ("AD3113" 60 :null "03/01/1982" :null)
	      ("AD3113" 60 :null "04/01/1982" :null)
	      ("AD3113" 60 :null "09/01/1982" :null)
	      ("AD3113" 70 :null "09/01/1982" :null)
	      ("AD3113" 70 :null "10/15/1982" :null)
	      ("IF1000" 10 :null "06/01/1982" :null)
	      ("IF1000" 90 :null "10/01/1982" :null)
	      ("IF1000" 100 :null "10/01/1982" :null)
	      ("IF2000" 10 :null "01/01/1982" :null)
	      ("IF2000" 100 :null "01/01/1982" :null)
	      ("IF2000" 100 :null "03/01/1982" :null)
	      ("IF2000" 110 :null "03/01/1982" :null)
	      ("IF2000" 110 :null "10/01/1982" :null)
	      ("MA2100" 10 :null "01/01/1982" :null)
	      ("MA2100" 20 :null "01/01/1982" :null)
	      ("MA2110" 10 :null "01/01/1982" :null)
	      ("MA2111" 50 :null "01/01/1982" :null)
	      ("MA2111" 60 :null "06/15/1982" :null)
	      ("MA2111" 40 :null "01/01/1982" :null)
	      ("MA2112" 60 :null "01/01/1982" :null)
	      ("MA2112" 180 :null "07/15/1982" :null)
	      ("MA2112" 70 :null "06/01/1982" :null)
	      ("MA2112" 70 :null "01/01/1982" :null)
	      ("MA2112" 80 :null "10/01/1982" :null)
	      ("MA2113" 60 :null "07/15/1982" :null)
	      ("MA2113" 80 :null "01/01/1982" :null)
	      ("MA2113" 70 :null "04/01/1982" :null)
	      ("MA2113" 80 :null "10/01/1982" :null)
	      ("MA2113" 180 :null "10/01/1982" :null)
	      ("OP1000" 10 :null "01/01/1982" :null)
	      ("OP1010" 10 :null "01/01/1982" :null)
	      ("OP1010" 130 :null "01/01/1982" :null)
	      ("OP2010" 10 :null "01/01/1982" :null)
	      ("OP2011" 140 :null "01/01/1982" :null)
	      ("OP2011" 150 :null "01/01/1982" :null)
	      ("OP2012" 140 :null "01/01/1982" :null)
	      ("OP2012" 160 :null "01/01/1982" :null)
	      ("OP2013" 140 :null "01/01/1982" :null)
	      ("OP2013" 170 :null "01/01/1982" :null)
	      ("PL2100" 30 :null "01/01/1982" :null)))))

;;; PROJECT table
;;; Desc:	Project number, Project name, Department responsible, Employee responsible, Estimated mean staffing, Estimated start date,
;;; Estimated end date, Major project - for a subproject
;;; Name: 	PROJNO PROJNAME 		DEPTNO RESPEMP PRSTAFF PRSTDATE PRENDATE MAJPROJ
;;; Type:	CHAR(6) VARCHAR(24) 		CHAR(3) CHAR(6) DEC(5,2) DATE		DATE		CHAR(6)
;;; 	       NOT NULL NOT NULL	      NOT NULL  NOT NULL		
;;; Values:	AD3100	ADMIN SERVICES		D01	000010	6.5	1982-01-01	1983-02-01	-
;;; 	 	AD3110	GENERAL ADMIN SYSTEMS	D21	000070	6	1982-01-01	1983-02-01	AD3100
;;; 	 	AD3111	PAYROLL PROGRAMMING	D21	000230	2	1982-01-01	1983-02-01	AD3110
;;; 	 	AD3112	PERSONNEL PROGRAMMING	D21	000250	1	1982-01-01	1983-02-01	AD3110
;;; 	 	AD3113	ACCOUNT PROGRAMMING	D21	000270	2	1982-01-01	1983-02-01	AD3110
;;; 	 	IF1000	EXECUTE SERVICES		C01	000030	2	1982-01-01	1983-02-01	-
;;; 	 	IF2000	USER EDUCATION		C01	000030	1	1982-01-01	1983-02-01	-
;;; 	 	MA2100	WELD LINE AUTOMATION	D01	000010	12	1982-01-01	1983-02-01	-
;;; 	 	MA2110	W L PROGRAMMING		D11	000060	9	1982-01-01	1983-02-01	MA2100
;;; 	 	MA2111	W L PROGRAM DESIGN	D11	000220	2	1982-01-01	1982-12-01	MA2110
;;; 	 	MA2112	W L ROBOT DESIGN	D11	000150	3	1982-01-01	1982-12-01	MA2110
;;; 	 	MA2113	W L PROD CONT PROGS	D11	000160	3	1982-02-15	1982-12-01	MA2110
;;; 	 	OP1000	OPERATION SUPPORT	E01	000050	6	1982-01-01	1983-02-01	-
;;; 	 	OP1010	OPERATION		E11	000090	5	1982-01-01	1983-02-01	OP1000
;;; 	 	OP2000	GEN SYSTEMS SERVICES	E01	000050	5	1982-01-01	1983-02-01	-
;;; 	 	OP2010	SYSTEMS SUPPORT		E21	000100	4	1982-01-01	1983-02-01	OP2000
;;; 	 	OP2011	SCP SYSTEMS SUPPORT	E21	000320	1	1982-01-01	1983-02-01	OP2010
;;; 	 	OP2012	APPLICATIONS SUPPORT	E21	000330	1	1982-01-01	1983-02-01	OP2010
;;; 	 	OP2013	DB/DC SUPPORT		E21	000340	1	1982-01-01	1983-02-01	OP2010
;;; 	 	PL2100	WELD LINE PLANNING	B01	000020	1	1982-01-01	1982-09-15	MA2100

(unless (table-exists-p 'project)
  (execute (:create-table 
	    'project
	    ((projno   :type text)
	     (projname :type text)
	     (deptno   :type text)
	     (respemp  :type text)
	     (prstaff  :type numeric)
	     (prstdate :type date)
	     (prendate :type date)
	     (majproj  :type (or text db-null)))))

  (execute (:insert-rows-into
	    'project
	    :columns 'projno 'projname 'deptno 'respemp 'prstaff 'prstdate 'prendate 'majproj
	    :values
	    '(("AD3100" "ADMIN SERVICES" "D01" "000010" 6.5 "1982-01-01" "1983-02-01" :null)
	      ("AD3110" "GENERAL ADMIN SYSTEMS" "D21" "000070" 6 "1982-01-01" "1983-02-01" "AD3100")
	      ("AD3111" "PAYROLL PROGRAMMING" "D21" "000230" 2 "1982-01-01" "1983-02-01" "AD3110")
	      ("AD3112" "PERSONNEL PROGRAMMING" "D21" "000250" 1 "1982-01-01" "1983-02-01" "AD3110")
	      ("AD3113" "ACCOUNT PROGRAMMING" "D21" "000270" 2 "1982-01-01" "1983-02-01" "AD3110")
	      ("IF1000" "EXECUTE SERVICES" "C01" "000030" 2 "1982-01-01" "1983-02-01" :null)
	      ("IF2000" "USER EDUCATION" "C01" "000030" 1 "1982-01-01" "1983-02-01" :null)
	      ("MA2100" "WELD LINE AUTOMATION" "D01" "000010" 12 "1982-01-01" "1983-02-01" :null)
	      ("MA2110" "W L PROGRAMMING" "D11" "000060" 9 "1982-01-01" "1983-02-01" "MA2100")
	      ("MA2111" "W L PROGRAM DESIGN" "D11" "000220" 2 "1982-01-01" "1982-12-01" "MA2110")
	      ("MA2112" "W L ROBOT DESIGN" "D11" "000150" 3 "1982-01-01" "1982-12-01" "MA2110")
	      ("MA2113" "W L PROD CONT PROGS" "D11" "000160" 3 "1982-02-15" "1982-12-01" "MA2110")
	      ("OP1000" "OPERATION SUPPORT" "E01" "000050" 6 "1982-01-01" "1983-02-01" :null)
	      ("OP1010" "OPERATION" "E11" "000090" 5 "1982-01-01" "1983-02-01" "OP1000")
	      ("OP2000" "GEN SYSTEMS SERVICES" "E01" "000050" 5 "1982-01-01" "1983-02-01" :null)
	      ("OP2010" "SYSTEMS SUPPORT" "E21" "000100" 4 "1982-01-01" "1983-02-01" "OP2000")
	      ("OP2011" "SCP SYSTEMS SUPPORT" "E21" "000320" 1 "1982-01-01" "1983-02-01" "OP2010")
	      ("OP2012" "APPLICATIONS SUPPORT" "E21" "000330" 1 "1982-01-01" "1983-02-01" "OP2010")
	      ("OP2013" "DB/DC SUPPORT" "E21" "000340" 1 "1982-01-01" "1983-02-01" "OP2010")
	      ("PL2100" "WELD LINE PLANNING" "B01" "000020" 1 "1982-01-01" "1982-09-15" "MA2100")))))

;;; SALES table
;;; Desc:	Date of sales	Employee's last name	Region of sales	Number of sales
;;; Name: 	SALES_DATE 	SALES_PERSON 	REGION 		SALES
;;; Type:	DATE		VARCHAR(15)	VARCHAR(15)	INTEGER
;;; Values:	12/31/2005	LUCCHESSI	Ontario-South	1
;;; 	 	12/31/2005	LEE		Ontario-South	3
;;; 	 	12/31/2005	LEE		Quebec		1
;;; 	 	12/31/2005	LEE		Manitoba	2
;;; 	 	12/31/2005	GOUNOT		Quebec		1
;;; 	 	03/29/2006	LUCCHESSI	Ontario-South	3
;;; 	 	03/29/2006	LUCCHESSI	Quebec		1
;;; 	 	03/29/2006	LEE		Ontario-South	2
;;; 	 	03/29/1996	LEE		Ontario-North	2
;;; 	 	03/29/2006	LEE		Quebec		3
;;; 	 	03/29/2006	LEE		Manitoba	5
;;; 	 	03/29/2006	GOUNOT		Ontario-South	3
;;; 	 	03/29/2006	GOUNOT		Quebec		1
;;; 	 	03/29/2006	GOUNOT		Manitoba	7
;;; 	 	03/30/2006	LUCCHESSI	Ontario-South	1
;;; 	 	03/30/2006	LUCCHESSI	Quebec		2
;;; 	 	03/30/2006	LUCCHESSI	Manitoba	1
;;; 	 	03/30/2006	LEE		Ontario-South	7
;;; 	 	03/30/2006	LEE		Ontario-North	3
;;; 	 	03/30/2006	LEE		Quebec		7
;;; 	 	03/30/2006	LEE		Manitoba	4
;;; 	 	03/30/2006	GOUNOT		Ontario-South	2
;;; 	 	03/30/2006	GOUNOT		Quebec		18
;;; 	 	03/30/2006	GOUNOT		Manitoba	1
;;; 	 	03/31/2006	LUCCHESSI	Manitoba	1
;;; 	 	03/31/2006	LEE		Ontario-South	14
;;; 	 	03/31/2006	LEE		Ontario-North	3
;;; 	 	03/31/2006	LEE		Quebec		7
;;; 	 	03/31/2006	LEE		Manitoba	3
;;; 	 	03/31/2006	GOUNOT		Ontario-South	2
;;; 	 	03/31/2006	GOUNOT		Quebec		1
;;; 	 	04/01/2006	LUCCHESSI	Ontario-South	3
;;; 	 	04/01/2006	LUCCHESSI	Manitoba	1
;;; 	 	04/01/2006	LEE		Ontario-South	8
;;; 	 	04/01/2006	LEE		Ontario-North	-
;;; 	 	04/01/2006	LEE		Quebec		8
;;; 	 	04/01/2006	LEE		Manitoba	9
;;; 	 	04/01/2006	GOUNOT		Ontario-South	3
;;; 	 	04/01/2006	GOUNOT		Ontario-North	1
;;; 	 	04/01/2006	GOUNOT		Quebec		3
;;; 	 	04/01/2006	GOUNOT		Manitoba	7

(unless (table-exists-p 'sales)
  (execute (:create-table 
	    'sales
	    ((sales_date   :type date)
	     (sales_person :type text)
	     (region       :type text)
	     (sales        :type integer))))

  (execute (:insert-rows-into
	    'sales
	    :columns 'sales_date 'sales_person 'region 'sales
	    :values '(("12/31/2005" "LUCCHESSI" "Ontario-South" 1)
		      ("12/31/2005" "LEE" "Ontario-South" 3)
		      ("12/31/2005" "LEE" "Quebec" 1)
		      ("12/31/2005" "LEE" "Manitoba" 2)
		      ("12/31/2005" "GOUNOT" "Quebec" 1)
		      ("03/29/2006" "LUCCHESSI" "Ontario-South" 3)
		      ("03/29/2006" "LUCCHESSI" "Quebec" 1)
		      ("03/29/2006" "LEE" "Ontario-South" 2)
		      ("03/29/1996" "LEE" "Ontario-North" 2)
		      ("03/29/2006" "LEE" "Quebec" 3)
		      ("03/29/2006" "LEE" "Manitoba" 5)
		      ("03/29/2006" "GOUNOT" "Ontario-South" 3)
		      ("03/29/2006" "GOUNOT" "Quebec" 1)
		      ("03/29/2006" "GOUNOT" "Manitoba" 7)
		      ("03/30/2006" "LUCCHESSI" "Ontario-South" 1)
		      ("03/30/2006" "LUCCHESSI" "Quebec" 2)
		      ("03/30/2006" "LUCCHESSI" "Manitoba" 1)
		      ("03/30/2006" "LEE" "Ontario-South" 7)
		      ("03/30/2006" "LEE" "Ontario-North" 3)
		      ("03/30/2006" "LEE" "Quebec" 7)
		      ("03/30/2006" "LEE" "Manitoba" 4)
		      ("03/30/2006" "GOUNOT" "Ontario-South" 2)
		      ("03/30/2006" "GOUNOT" "Quebec" 18)
		      ("03/30/2006" "GOUNOT" "Manitoba" 1)
		      ("03/31/2006" "LUCCHESSI" "Manitoba" 1)
		      ("03/31/2006" "LEE" "Ontario-South" 14)
		      ("03/31/2006" "LEE" "Ontario-North" 3)
		      ("03/31/2006" "LEE" "Quebec" 7)
		      ("03/31/2006" "LEE" "Manitoba" 3)
		      ("03/31/2006" "GOUNOT" "Ontario-South" 2)
		      ("03/31/2006" "GOUNOT" "Quebec" 1)
		      ("04/01/2006" "LUCCHESSI" "Ontario-South" 3)
		      ("04/01/2006" "LUCCHESSI" "Manitoba" 1)
		      ("04/01/2006" "LEE" "Ontario-South" 8)
		      ("04/01/2006" "LEE" "Ontario-North" 0)
		      ("04/01/2006" "LEE" "Quebec" 8)
		      ("04/01/2006" "LEE" "Manitoba" 9)
		      ("04/01/2006" "GOUNOT" "Ontario-South" 3)
		      ("04/01/2006" "GOUNOT" "Ontario-North" 1)
		      ("04/01/2006" "GOUNOT" "Quebec" 3)
		      ("04/01/2006" "GOUNOT" "Manitoba" 7)))))

;;; STAFF table
;;; Desc:	Employee number, Employee name, Department number, Job type, Years of service, Current salary, Commission
;;; Name: 	ID 	NAME 	DEPT 	JOB 	YEARS 	SALARY 	COMM
;;; Type:	SMALLINT VARCHAR(9)   SMALLINT	CHAR(5)	SMALLINT DECIMAL(7,2)	DECIMAL(7,2)
;;; 		NOT NULL
;;; Values:	10	Sanders		20	Mgr	7	18357.50	-
;;; 	 	20	Pernal		20	Sales	8	18171.25	612.45
;;; 	 	30	Marenghi	38	Mgr	5	17506.75	-
;;; 	 	40	O'Brien		38	Sales	6	18006.00	846.55
;;; 	 	50	Hanes		15	Mgr	10	20659.80	-
;;; 	 	60	Quigley		38	Sales	-	16808.30	650.25
;;; 	 	70	Rothman		15	Sales	7	16502.83	1152.00
;;; 	 	80	James		20	Clerk	-	13504.60	128.20
;;; 	 	90	Koonitz		42	Sales	6	18001.75	1386.70
;;; 	 	100	Plotz		42	Mgr	7	18352.80	-
;;; 	 	110	Ngan		15	Clerk	5	12508.20	206.60
;;; 	 	120	Naughton	38	Clerk	-	12954.75	180.00
;;; 	 	130	Yamaguchi	42	Clerk	6	10505.90	75.60
;;; 	 	140	Fraye		51	Mgr	6	21150.00	-
;;; 	 	150	Williams	51	Sales	6	19456.50	637.65
;;; 	 	160	Molinare	10	Mgr	7	22959.20	-
;;; 	 	170	Kermisch	15	Clerk	4	12258.50	110.10
;;; 	 	180	Abrahams	38	Clerk	3	12009.75	236.50
;;; 	 	190	Sneider		20	Clerk	8	14252.75	126.50
;;; 	 	200	Scoutten	42	Clerk	-	11508.60	84.20
;;; 	 	210	Lu		10	Mgr	10	20010.00	-
;;; 	 	220	Smith		51	Sales	7	17654.50	992.80
;;; 	 	230	Lundquist	51	Clerk	3	13369.80	189.65
;;; 	 	240	Daniels		10	Mgr	5	19260.25	-
;;; 	 	250	Wheeler		51	Clerk	6	14460.00	513.30
;;; 	 	260	Jones		10	Mgr	12	21234.00	-
;;; 	 	270	Lea		66	Mgr	9	18555.50	-
;;; 	 	280	Wilson		66	Sales	9	18674.50	811.50
;;; 	 	290	Quill		84	Mgr	10	19818.00	-
;;; 	 	300	Davis		84	Sales	5	15454.50	806.10
;;; 	 	310	Graham		66	Sales	13	21000.00	200.30
;;; 	 	320	Gonzales	66	Sales	4	16858.20	844.00
;;; 	 	330	Burke		66	Clerk	1	10988.00	55.50
;;; 	 	340	Edwards		84	Sales	7	17844.00	1285.00
;;; 	 	350	Gafney		84	Clerk	5	13030.50	188.00

(unless (table-exists-p 'staff)
  (execute (:create-table 
	    'staff
	    ((id     :type smallint)
	     (name   :type text)
	     (dept   :type smallint)
	     (job    :type text)
	     (years  :type (or smallint db-null))
	     (salary :type numeric)
	     (comm   :type (or numeric db-null)))))

  (execute (:insert-rows-into
	    'staff
	    :columns 'id 'name 'dept 'job 'years 'salary 'comm
	    :values
	    '((10 "Sanders" 20 "Mgr" 7 18357.50 :null)
	      (20 "Pernal" 20 "Sales" 8 18171.25 612.45)
	      (30 "Marenghi" 38 "Mgr" 5 17506.75 :null)
	      (40 "O'Brien" 38 "Sales" 6 18006.00 846.55)
	      (50 "Hanes" 15 "Mgr" 10 20659.80 :null)
	      (60 "Quigley" 38 "Sales" :null 16808.30 650.25)
	      (70 "Rothman" 15 "Sales" 7 16502.83 1152.00)
	      (80 "James" 20 "Clerk" :null 13504.60 128.20)
	      (90 "Koonitz" 42 "Sales" 6 18001.75 1386.70)
	      (100 "Plotz" 42 "Mgr" 7 18352.80 :null)
	      (110 "Ngan" 15 "Clerk" 5 12508.20 206.60)
	      (120 "Naughton" 38 "Clerk" :null 12954.75 180.00)
	      (130 "Yamaguchi" 42 "Clerk" 6 10505.90 75.60)
	      (140 "Fraye" 51 "Mgr" 6 21150.00 :null)
	      (150 "Williams" 51 "Sales" 6 19456.50 637.65)
	      (160 "Molinare" 10 "Mgr" 7 22959.20 :null)
	      (170 "Kermisch" 15 "Clerk" 4 12258.50 110.10)
	      (180 "Abrahams" 38 "Clerk" 3 12009.75 236.50)
	      (190 "Sneider" 20 "Clerk" 8 14252.75 126.50)
	      (200 "Scoutten" 42 "Clerk" :null 11508.60 84.20)
	      (210 "Lu" 10 "Mgr" 10 20010.00 :null)
	      (220 "Smith" 51 "Sales" 7 17654.50 992.80)
	      (230 "Lundquist" 51 "Clerk" 3 13369.80 189.65)
	      (240 "Daniels" 10 "Mgr" 5 19260.25 :null)
	      (250 "Wheeler" 51 "Clerk" 6 14460.00 513.30)
	      (260 "Jones" 10 "Mgr" 12 21234.00 :null)
	      (270 "Lea" 66 "Mgr" 9 18555.50 :null)
	      (280 "Wilson" 66 "Sales" 9 18674.50 811.50)
	      (290 "Quill" 84 "Mgr" 10 19818.00 :null)
	      (300 "Davis" 84 "Sales" 5 15454.50 806.10)
	      (310 "Graham" 66 "Sales" 13 21000.00 200.30)
	      (320 "Gonzales" 66 "Sales" 4 16858.20 844.00)
	      (330 "Burke" 66 "Clerk" 1 10988.00 55.50)
	      (340 "Edwards" 84 "Sales" 7 17844.00 1285.00)
	      (350 "Gafney" 84 "Clerk" 5 13030.50 188.00)))))

;;; PRODUCT table
;;; Name: 	PID 		NAME 					PRICE 	    PROMOPRICE 	PROMOSTART 	PROMOEND 	DESCRIPTION
;;; Type:	VARCHAR(10) 	VARCHAR(128)			    DECIMAL(30,2) DECIMAL(30,2) DATE		DATE		XML
;;; Values:	100-100-01	Snow Shovel, Basic 22 inch		9.99		7.25	11/19/2004	12/19/2004	p1.xml
;;; 	 	100-101-01	Snow Shovel, Deluxe 24 inch		19.99		15.99	12/18/2005	02/28/2006	p2.xml
;;; 	 	100-103-01	Snow Shovel, Super Deluxe 26 inch	49.99		39.99	12/22/2005	02/22/2006	p3.xml
;;; 	 	100-201-01	Ice Scraper, Windshield 4 inch		3.99		--	-- 		--		p4.xml

(unless (table-exists-p 'product)
  (execute (:create-table 
	    'product
	    ((pid :type text)
	     (name :type text)
	     (price :type numeric)
	     (promoprice :type (or numeric db-null))
	     (promostart :type (or date db-null))
	     (promoend :type (or date db-null))
	     (description :type text))))

  (execute (:insert-rows-into
	    'product
	    :columns 'pid 'name 'price 'promoprice 'promostart 'promoend 'description
	    :values
	    '(("100-100-01" "Snow Shovel, Basic 22 inch" 9.99 7.25 "11/19/2004" "12/19/2004" "p1.xml")
	      ("100-101-01" "Snow Shovel, Deluxe 24 inch" 19.99 15.99 "12/18/2005" "02/28/2006" "p2.xml")
	      ("100-103-01" "Snow Shovel, Super Deluxe 26 inch" 49.99 39.99 "12/22/2005" "02/22/2006" "p3.xml")
	      ("100-201-01" "Ice Scraper, Windshield 4 inch" 3.99 :null :null :null "p4.xml")))))

;;; PURCHASEORDER table
;;; Name:	POID 	STATUS  CUSTID  ORDERDATE 	PORDER  COMMENTS
;;; Type:	BIGINT NOT NULL	VARCHAR(10) NOT NULL	BIGINT	DATE	XML	VARCHAR(1000)
;;; Values:	5000	Unshipped	1002	02/18/2006	po1.xml	THIS IS A NEW PURCHASE ORDER
;;;	  	5001	Shipped	1003	02/03/2005	po2.xml	THIS IS A NEW PURCHASE ORDER
;;;	  	5002	Shipped	1001	02/29/2004	po3.xml	THIS IS A NEW PURCHASE ORDER
;;;	  	5003	Shipped	1002	02/28/2005	po4.xml	THIS IS A NEW PURCHASE ORDER
;;;	  	5004	Shipped	1005	11/18/2005	po5.xml	THIS IS A NEW PURCHASE ORDER
;;;	  	5006	Shipped	1002	03/01/2006	po6.xml	THIS IS A NEW PURCHASE ORDER

(unless (table-exists-p 'purchaseorder)
  (execute (:create-table 
	    'purchaseorder
	    ((poid      :type integer)
	     (status    :type text)
	     (custid    :type integer)
	     (orderdate :type date)
	     (porder    :type text)
	     (comments  :type text))))

  (execute (:insert-rows-into
	    'purchaseorder
	    :columns 'poid 'status 'custid 'orderdate 'porder 'comments
	    :values
	    '((5000 "Unshipped" 1002 "02/18/2006" "po1.xml" "THIS IS A NEW PURCHASE ORDER")
	      (5001 "Shipped" 1003 "02/03/2005" "po2.xml" "THIS IS A NEW PURCHASE ORDER")
	      (5002 "Shipped" 1001 "02/29/2004" "po3.xml" "THIS IS A NEW PURCHASE ORDER")
	      (5003 "Shipped" 1002 "02/28/2005" "po4.xml" "THIS IS A NEW PURCHASE ORDER")
	      (5004 "Shipped" 1005 "11/18/2005" "po5.xml" "THIS IS A NEW PURCHASE ORDER")
	      (5006 "Shipped" 1002 "03/01/2006" "po6.xml" "THIS IS A NEW PURCHASE ORDER")))))

;;; CUSTOMER table
;;; Name: CID INFO
;;; Type:	BIGINT NOT NULL	XML
;;; Values:	1000	c1.xml
;;; 	 	1001	c2.xml
;;; 	 	1002	c3.xml
;;; 	 	1003	c4.xml
;;; 	 	1004	c5.xml
;;; 	 	1005	c6.xml

(unless (table-exists-p 'customer)
  (execute (:create-table 
	    'customer
	    ((cid :type bigint)
	     (info :type text))))

  (execute (:insert-rows-into
	    'customer
	    :columns 'cid 'info
	    :values
	    '((1000 "c1.xml")
	      (1001 "c2.xml")
	      (1002 "c3.xml")
	      (1003 "c4.xml")
	      (1004 "c5.xml")
	      (1005 "c6.xml")))))

;; CATALOG table
;; Name:	NAME 		CATALOG
;; Type:	VARCHAR(128)	XML
;; 		NOT NULL       

(unless (table-exists-p 'catalog)
  (execute (:create-table 
	    'catalog
	    ((name :type text)
	     (info :type xml)))))

(defparameter *catalog-xml* "catalog.xml")

;;; INVENTORY table
;;; Name: 	PID 	     QUANTITY 	LOCATION
;;; Type:	VARCHAR(10)  INTEGER	VARCHAR(128)
;;; 		NOT NULL
;;; Values:	100-100-01	5	--
;;;	  	100-101-01	25	Store
;;;	  	100-103-01	55	Store
;;;	  	100-201-01	99	Warehouse

(unless (table-exists-p 'inventory)
  (execute (:create-table 
	    'inventory
	    ((pid :type text)
	     (quantity :type integer)
	     (location :type (or text db-null)))))

  (execute (:insert-rows-into
	    'inventory
	    :columns 'pid 'quantity 'location
	    :values
	    '(("100-100-01" 5 :null)
	      ("100-101-01" 25 "Store")
	      ("100-103-01" 55 "Store")
	      ("100-201-01" 99 "Warehouse")))))

;;; PRODUCTSUPPLIER table
;;; Name: 	PID 		SID
;;; Type:	VARCHAR(10)	VARCHAR(10) 
;;; 	 	NOT NULL 	NOT NULL
;;; Values:	100-101-01	100
;;;	  	100-201-01	101

(unless (table-exists-p 'productsupplier)
  (execute (:create-table 
	    'productsupplier
	    ((pid :type text)
	     (sid :type text))))

  (execute (:insert-rows-into
	    'productsupplier
	    :columns 'pid 'sid
	    :values
	    '(("100-101-01" 100)
	      ("100-201-01" 101)))))

;;; SUPPLIERS table
;;; Name:	SID 		ADDR
;;; Type:	VARCHAR(10)	XML
;;; 	 	NOT NULL
;;; Values:	100		s1.xml
;;;	  	101		s2.xml

(unless (table-exists-p 'suppliers)
  (execute (:create-table 
	    'suppliers
	    ((sid :type text)
	     (addr :type text))))

  (execute (:insert-rows-into
	    'suppliers
	    :columns 'sid 'addr
	    :values
	    '(("100" "s1.xml")
	      ("101" "s2.xml")))))
