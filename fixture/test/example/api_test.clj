(ns example.api-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [example.api :as api]))

(def ^:dynamic *bound-var-once* nil)
(def ^:dynamic *bound-var-each* nil)

(use-fixtures :once (fn [test]
                      (binding [*bound-var-once* 1]
                        (test))))

(use-fixtures :each (fn [test]
                      (binding [*bound-var-each* 2]
                        (test))))

(deftest api-test
  (is (= 2 (api/foo 1))))

(deftest once-bound-var-test
  (is (= 1 *bound-var-once*)))

(deftest each-bound-var-test
  (is (= 2 *bound-var-each*)))
