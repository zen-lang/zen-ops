(ns json-patch.core-test
  (:require [json-patch.core :as sut]
            [cheshire.core]
            [clojure.test :refer :all]))

(defn not-nil? [x] (not (nil? x)))

(defmacro is= [a b]
  `(do (is (= ~a ~b)) ~b))

(defmacro is-match [obj patch exp]
  `(let [res# (sut/json-patch ~obj ~patch)]
     (is (= (:success res#) ~exp))
     res#))

(defmacro is-error [obj patch &[err]]
  (if err
    `(let [res# (sut/json-patch ~obj ~patch)]
       (is (= (:errors res#) ~err))
       res#)

    `(let [res# (sut/json-patch ~obj ~patch)]
       (is (:errors res#))
       res#)))

(deftest path2-test
  (testing "utils"

    (is= 4 (sut/*get-in {:a {:b {:c [{:d 3} {:d 4}]}}} [:a :b :c 1 :d]))

    (is= nil (sut/*get-in {:a "ups"} [:a 1]))

    (is (thrown? Exception (sut/*assoc-in {:a "ups"} [:a 1] 5)))

    (sut/*get-in {:a 1} []))


  (testing "patch"
    (is-match
     {:a "v1"} [{:op "replace" :path "/a" :value "v2"}]
     {:a "v2"})

    (is-match
     {:baz "qux" :foo "bar"}
     [{:op "replace", :path "/baz", :value "boo" },
      {:op "add", :path "/hello", :value ["world"] },
      {:op "remove", :path "/foo" }]
     {:baz "boo",
      :hello ["world"]})

    (is-error
     {:a "v1"} [{:op "replace" :path "/b" :value "v2"}]
     ["Value to replace does not present @ /b"])

    (is-match
     {:address [{:line ["a"]}]}
     [{:op "replace", :path "/address/0/line/0", :value "b"}]
     {:address [{:line ["b"]}]})

    (is-error
     {:name [{:family "ddd"}]}
     [{:op "replace", :path "/name/0/family/0", :value "bbb"}]
     ["Value to replace does not present @ /name/0/family/0"])

    (is-match
     {:address [{:line ["a"]}]}
     [{:op "replace", :path "/address/0/line", :value ["b" "c"]}]
     {:address [{:line ["b" "c"]}]})


    (is-match
     {:foo nil}
     [{:op "add", :path "/foo", :value 1}]
     {:foo 1})

    (is-error
     []
     [{:op "add", :path "/foo", :value 1}]
     ["Should be index in array, got :foo @ /foo"])

    (is-error
     {:foo nil}
     [{:op "add", :path "/foo"}]
     ["Missing value in {\"op\":\"add\",\"path\":\"/foo\"}"])

    (is-error
     {:foo nil}
     [{:op "add", :value 1}]
     ["Missing path in {\"op\":\"add\",\"value\":1}"])

    (is-match
     "foo" [{:op "replace", :path "", :value "bar"}]
     "bar")

    (is-match
     [] [{:op "add", :path "/-", :value "hi"}]
     ["hi"])

    (is-match
     {:foo 1, :bar [1 2 3 4]} [{:op "remove", :path "/bar"}]
     {:foo 1})

    (is-match
     {:foo nil}
     [{:op "replace", :path "/foo", :value "truthy"}]
     {:foo "truthy"})

    (is-match
     {:foo nil}
     [{:op "move", :from "/foo", :path "/bar"}]
     {:bar nil})

    (is-match
     {:foo "bar"}
     [{:op "test", :path "/foo" :value "bar"}]
     {:foo "bar"})

    (is-match
     ["bar" "buz"]
     [{:op "test", :path "/0" :value "bar"}
      {:op "test", :path "/1" :value "buz"}]
     ["bar" "buz"])

    (is-error
     ["bar" "buz"]
     [{:op "replace", :path "/4" :value "bar"}])

    (def kulib (reduce-kv
                (fn [acc k v]
                  (assoc acc (keyword k) v)
                  ) {}
                {"e^f"     3,
                 "i\\j"    5,
                 "g|h"     4,
                 ""       0
                 "foo"     ["bar" "baz"]
                 " "        7,
                 "m~n"      8,
                 "a/b"     1,
                 "k\"l"      6,
                 "c%d"     2}))

    (is-match kulib
     [{:op "test", :path "/foo",  :value ["bar" "baz"]}
      {:op "test", :path "/foo/0", :value "bar"}
      {:op "test", :path "/", :value 0}
      {:op "test", :path "/a~1b", :value 1}
      {:op "test", :path "/c%d", :value 2}
      {:op "test", :path "/e^f", :value 3}
      {:op "test", :path "/g|h", :value 4}
      {:op "test", :path "/i\\j", :value 5}
      {:op "test", :path "/k\"l", :value 6}
      {:op "test", :path "/ ", :value 7}
      {:op "test", :path "/m~0n", :value 8}]
     kulib)

    (is-match [1 2 3 4]
              [{:op "remove", :path "/1"} {:op "remove", :path "/2"}]
              [1 3])

    (is-match {:foo 1, :baz [{:qux "hello"}]}
              [{:op "remove", :path "/baz/0/qux"}]
              {:foo 1, :baz [{}]})

    (is-match
     {:address [{:line ["a"]}]}
     [{:op "add", :path "/address/0/line/-", :value "b"}]
     {:address [{:line ["a" "b"]}]})

    (is-match
     {:address [{:line ["a"]}]}
     [{:op "add", :path "/address/0/line", :value "b"}]
     {:address [{:line "b"}]})

    (is-error
     {:address [{:line ["a"]}]}
     [{:op "add", :path "/address/0/line/", :value "b"}]
     ["Should be index in array, got : @ /address/0/line/"])

    (is-error ["foo" "bar"]
              [{:op "test", :path "/00", :value "foo"}])

    (is-match
     {:a 1 :b 2 :d {:f 1} :e {:f [1 2]} :col [{:a 2} {:a 1}]}
     [{:op "replace", :path "/a", :value 3}
      {:op "replace", :path "/b", :value 4}
      {:op "remove", :path "/d"}
      {:op "remove", :path "/e/f/0"}
      {:op "move", :from "/col/0", :path "/col/1"}]
     {:a 3 :b 4 :e {:f [2]} :col [{:a 1} {:a 2}]})

    (is-match
     {}
     [{:op "add-if-null", :path "/foo", :value []}
      {:op "add", :path "/foo/-", :value 1}
      {:op "add", :path "/foo/-", :value 2}
      {:op "add", :path "/foo/-", :value 3}]
     {:foo [1 2 3]})


    (is-match
     ["all" "grass" "cows" "eat" "slowly"]
     [{:from "/1", :op "move", :path "/3"}]
     ["all" "cows" "eat" "grass" "slowly"])

    (do
      (def spec-tests (into
                       (cheshire.core/parse-string (slurp "test/json_patch/tests.json") keyword)
                       (cheshire.core/parse-string (slurp "test/json_patch/spec_tests.json") keyword)))
      (doseq [spec spec-tests]
        (let [desc (if (contains? spec :expected)
                     (str "  (is-match " (pr-str (:doc spec)) "\n     " (pr-str (:patch spec)) "\n     "
                          (pr-str (:expected spec)) ")")
                     (str "  (is-error " (pr-str (:doc spec)) "\n     " (pr-str (:patch spec)) ")"))]
          (println " # " (or (:comment spec) (:error spec) (:patch spec)) "\n" desc)

          (when-not (:disabled spec)
            (testing (str (:comment spec) "\n" desc)
              (let [res (sut/json-patch (:doc spec) (:patch spec))]
                (cond
                  (:expected spec)
                  (is-match
                   (:doc spec)
                            (:patch spec)
                            (:expected spec))

                  (:error spec)
                  (testing (:error spec)
                    (is-error (:doc spec) (:patch spec)))))))))

      (println "SPECS: " (count spec-tests)))

    )


  (is (= (sut/*get-in {:a (map (fn [x] {:b x})[1 3 4])} [:a 1 :b]) 3))
  





  )


