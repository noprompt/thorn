(ns thorn.core-test
  (:require [clojure.test :refer :all]
            [thorn.core :as thorn]))

(deftest util-test
  (testing "unit->string"
    (is (= (thorn/unit->string
            {:tag :unit
             :content [{:tag :number :content [1]}
                       {:tag :numerator_units :content ["px"]}]})
           "1px"))
    (is (= (thorn/unit->string
            {:tag :unit
             :content [{:tag :number :content [1]}
                       {:tag :numerator_units :content ["px"]}
                       {:tag :denominator_units :content ["em"]}]})
           "1px/em"))
    (is (= (thorn/unit->string
            {:tag :unit
             :content [{:tag :number :content [1/2]}
                       {:tag :numerator_units :content ["px"]}
                       {:tag :denominator_units :content ["em"]}]})
           "1px/2em")))

  (testing "safe-comma-split"
    (is (= (thorn/safe-comma-split "foo")
           '("foo")))
    (is (= (thorn/safe-comma-split "foo[x=\",\"]")
           '("foo[x=\",\"]")))
    (is (= (thorn/safe-comma-split "foo[x=\",\"], bar[y=\"a,b\"]")
           '("foo[x=\",\"]" " bar[y=\"a,b\"]")))
    (is (= (thorn/safe-comma-split "foo[x=\",\"], baz quux")
           '("foo[x=\",\"]" " baz quux")))
    (is (= (thorn/safe-comma-split "foo[x=\",\"] baz quux")
           '("foo[x=\",\"] baz quux")))))
