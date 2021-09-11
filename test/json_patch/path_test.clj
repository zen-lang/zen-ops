(ns json-patch.path-test
  (:require [json-patch.path :as sut]
            [clojure.test :refer :all]))

(defmacro is= [a b]
  `(do (is (= ~a ~b)) ~b))

(deftest json-patch-path
  (testing "path"
    (is= [(keyword "")] (sut/parse-path "/"))

    (is= [:biscuits 1] (sut/parse-path "/biscuits/1"))
    (is= [:biscuits :jp/conj] (sut/parse-path "/biscuits/-"))

    (is= [:biscuits (keyword "~") :/]
         (sut/parse-path "/biscuits/~0/~1"))

    (is= [] (sut/parse-path ""))

    (is= [:biscuits 0 :name] (sut/parse-path "/biscuits/0/name"))))
