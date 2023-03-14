(ns tardigrade.circuit-breaker.counter-test
  (:require [clojure.test :refer :all]
            [tardigrade.circuit-breaker.counter :as sut]
            [tardigrade.circuit-breaker.protocols :as tcp]))

(deftest create-counter-trippable-test
  (testing "Trippable counter, Happy path"
    (let [a (sut/create-counter-trippable "test" {:timeout 2
                                                  :threshold 10
                                                  :failure-pred (fn [e]
                                                                  (= (.getMessage e)
                                                                     "hello"))})]
      (doseq [n (range 10)]
        (let [s (new java.io.StringWriter)]
          ;; Supress errors logging to std out
          (binding [*out* s]
            (tcp/run-body a
                          (fn []
                            (throw (Exception. "hello")))))))
      (is (tcp/tripped? a)
          "Circut should be tripped by now")

      (Thread/sleep 2000)
      (is (not (tcp/tripped? a))
          "Circut should be open again"))))
