(ns tardigrade.circuit-breaker.timer-test
  (:require [clojure.test :refer :all]
            [tardigrade.circuit-breaker.timer :as sut]
            [tardigrade.circuit-breaker.protocols :as tcp]))


(deftest create-trippable-timer-test
  (testing "Trippable sliding window, Happy path"
    (let [a (sut/create-timer-trippable "test" {:timeout 2
                                                :threshold 2
                                                :window-size 2
                                                :threshold-time 500
                                                :body-fn-timeout 1})]
      (let [[t res] (tcp/run-body a
                                  (fn []
                                    (Thread/sleep 1100)
                                    1))]
        (is (= t
               ::tcp/failure)
            "Body fn timeout assertion"))

      (is (= (tcp/run-body a
                           (fn []
                             (Thread/sleep 550)
                             1))
             [::tcp/success 1])
          "Timeout threshold assertion")

      (is (tcp/tripped? a))
      (Thread/sleep 2000)

      (is (not (tcp/tripped? a))
          "Tripped timeout"))))
