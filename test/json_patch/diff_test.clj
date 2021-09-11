(ns json-patch.diff-test
  (:require [json-patch.diff :as sut]
            [clojure.test :refer :all]))

(defmacro is= [a b]
  `(do (is (= ~a ~b)) ~a))

(defmacro diff? [a b op]
  `(is= (sut/diff ~a ~b) ~op))

;; SAFE DIFF
;; before replace test prev value

(deftest test-diff

  (diff? 1 2
       [{:op "replace" :path "" :value 2}])

  (diff? 1 1
       [])

  (diff? {} 2
       [{:op "replace" :path "" :value 2}])

  (diff? 2 {:a 1}
       [{:op "replace" :path "" :value {:a 1}}])

  (diff? {:a 1} {:a 2}
       [{:op "replace" :path "/a" :value 2}])

  (diff? {:a 1} {}
       [{:op "remove" :path "/a"}])

  (diff? {} {:a 1}
       [{:op "add" :path "/a" :value 1}])

  (diff? {:a 1} {:a 1}
       [])

  (diff? {:a {:b 1}} {:a {:b 1}}
       [])

  (diff? {:a {:b 1}} {:a {:b 2}}
       [{:op "replace", :path "/a/b", :value 2}])

  (diff? {:a {:b 1}} {:a {}}
       [{:op "remove", :path "/a/b"}])

  (diff? {:a {}} {:a {:b 2}}
       [{:op "add", :path "/a/b", :value 2}])

  (diff? {} {:a {:b 2}}
       [{:op "add", :path "/a", :value {:b 2}}])

  (diff? [1] [1]
       [])

  (diff? [1] [2]
         [{:op "replace", :path "/0", :value 2}])

  (diff? [1] []
         [{:op "remove", :path "/0"}])

  (diff? [] [2]
         [{:op "add", :path "/0" :value 2}])


  (diff? {:name [{:given ["John" "Jr"] :family "smith"}]}
         {:name [{:given ["Jr" "John" "Johny"] :family "Smith"}]}
         [{:op "replace", :path "/name/0/given/0", :value "Jr"}
          {:op "replace", :path "/name/0/given/1", :value "John"}
          {:op "add", :path "/name/0/given/2", :value "Johny"}
          {:op "replace", :path "/name/0/family", :value "Smith"}])

  (diff? {:name [{:given ["John" "Jr"] :family "smith"}]}
         {:name [{:given ["Jr" "John" "Johny"] :family "Smith" :use "home"}]}
         [{:op "replace", :path "/name/0/given/0", :value "Jr"}
          {:op "replace", :path "/name/0/given/1", :value "John"}
          {:op "add", :path "/name/0/given/2", :value "Johny"}
          {:op "replace", :path "/name/0/family", :value "Smith"}
          {:op "add", :path "/name/0/use", :value "home"}])

  (diff? {:name [{:given ["John" "Jr"  "Johny"] :family "smith"}]}
         {:name [{:given ["Jr" "John"] :family "Smith" :use "home"}]}

         [{:op "replace", :path "/name/0/given/0", :value "Jr"}
          {:op "replace", :path "/name/0/given/1", :value "John"}
          {:op "remove", :path "/name/0/given/2"}
          {:op "replace", :path "/name/0/family", :value "Smith"}
          {:op "add", :path "/name/0/use", :value "home"}]
         )

  )
