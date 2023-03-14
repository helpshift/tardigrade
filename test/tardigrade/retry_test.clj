(ns tardigrade.retry-test
  (:require [tardigrade.retry :refer :all]
            [clojure.test :refer :all]))

(deftest retry-test
  (let [num-retries (atom 0)]
    (is (thrown? AssertionError
                 (with-retry-fn [1 1 1]
                   (fn [e]
                     (= (class e) AssertionError))
                   (fn []
                     (swap! num-retries inc)
                     (throw (AssertionError.))))))
    (= num-retries 4)))
