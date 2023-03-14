(ns tardigrade.circuit-breaker.sliding-window-test
  (:require [clojure.test :refer :all]
            [tardigrade.circuit-breaker.sliding-window :as sut]
            [tardigrade.circuit-breaker.protocols :as tcp]))


(deftest create-sliding-window-trippable-test
  (testing "Trippable sliding window, Happy path"
    (let [a (sut/create-sliding-window-trippable "test" {:timeout 2
                                                         :threshold 10
                                                         :window-size 2
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
          "Circut should be open again")))

  (testing "Trippable sliding window, out of window error"
    (let [a (sut/create-sliding-window-trippable "test" {:timeout 2
                                                         :threshold 4
                                                         :window-size 3
                                                         :failure-pred (fn [e]
                                                                         (= (.getMessage e)
                                                                            "hello"))})]
      (let [s (new java.io.StringWriter)]
        ;; Supress errors logging to std out
        (binding [*out* s]
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))
          (Thread/sleep 1000)
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))
          (Thread/sleep 1000)
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))
          (Thread/sleep 1000)
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))))
      (is (not (tcp/tripped? a))
          "Circut should never be tripped")))


  (testing "Trippable sliding window, out of window error off by one"
    (let [a (sut/create-sliding-window-trippable "test" {:timeout 2
                                                         :threshold 2
                                                         :window-size 2
                                                         :failure-pred (fn [e]
                                                                         (= (.getMessage e)
                                                                            "hello"))})]
      (let [s (new java.io.StringWriter)]
        ;; Supress errors logging to std out
        (binding [*out* s]
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))
          (Thread/sleep 60000)
          (tcp/run-body a
                        (fn []
                          (throw (Exception. "hello"))))
          (Thread/sleep 1000)))
      (is (not (tcp/tripped? a))
          "Circut should never be tripped"))))
