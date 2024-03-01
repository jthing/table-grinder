(defpackage table-grinder/tests/main
  (:use :cl
        :table-grinder
        :rove))
(in-package :table-grinder/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :table-grinder)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
