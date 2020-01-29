(defpackage swarm/tests/main
  (:use :cl
        :swarm
        :rove))
(in-package :swarm/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :swarm)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
