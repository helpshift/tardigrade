(ns tardigrade.circuit-breaker.sliding-window
  (:require [clj-time.core :as time]
            [clj-time.coerce :as ctcc]
            [tardigrade.circuit-breaker.protocols :as tcp]))

(defn circuit-tripped?
  [v timeout]
  (let [dt (time/now)]
    (when-let [dt2 (:last-trip v)]
      (< (time/in-seconds (time/interval dt2
                                         dt))
         timeout))))


(defn trip-failure-hit
  [state window-size threshold]
  (swap! state
         (fn [v]
           (let [dt (time/now)
                 s-tick (int (/ (ctcc/to-long dt) 1000))
                 ix (mod s-tick window-size)
                 new-v (cond
                         ;; Count falls under the same second tick
                         (= (get v
                                 :last-tick)
                            s-tick)
                         (-> v
                             (update-in [:failures ix]
                                        (fnil inc 1))
                             (update-in [:total]
                                        (fnil inc 1)))

                         ;; Count falls under an out of range tick
                         (and (get v
                                   :last-tick)
                              (>= (- s-tick
                                     (get v
                                          :last-tick))
                                  window-size))

                         (-> v
                             (assoc :total 0)
                             (assoc :failures
                                    {})
                             (assoc :last-tick
                                    s-tick))

                         ;; Count falls under under the next second tick
                         :else
                         (-> v
                             (update-in [:total]
                                        (fn [c]
                                          (inc (- c (get-in v [:failures ix] 0)))))
                             (assoc-in [:failures ix]
                                       1)
                             (assoc :last-tick
                                    s-tick)))]
             (if (>= (:total new-v) threshold)
               (assoc new-v :last-trip dt)
               new-v)))))


(defn trip-failure-reset
  [state]
  (swap! state
         (fn [v]
           (-> v
               (assoc :failures
                      {})
               (assoc :last-trip
                      nil)
               (assoc :total
                      0)))))


(defrecord SlidingWindowCB
    [circuit-breaker-name state
     threshold timeout window-size
     threshold-time failure-pred]
  tcp/Trippable
  (tripped? [this]
    (circuit-tripped? @state timeout))
  (run-body [this body-fn]
    (try
      (let [res (body-fn)]
        [::tcp/success res])
      (catch Throwable e
        (if (failure-pred e)
          (do ;; (log :error
              ;;      "Exception supressed by circuit breaker"
              ;;      e
              ;;      {:circuit-breaker-name circuit-breaker-name})
              (trip-failure-hit state
                                window-size
                                threshold)
              [::tcp/failure e])
          (throw e))
        [::tcp/failure e])))
  (reset-trip [this]
    (trip-failure-reset state)))


(defn create-sliding-window-trippable
  "Creates a trippable record which counts errors on based on
  `failure-pred`. The count is a total of all errors that happen in a
  sliding window.

  Example.
  We have configured a sliding window trippable to trip after
  10 failures with window size of 3 seconds

  ----------------------------------------------------------------------
  t0
  ----------------------------------------------------------------------

  Three exceptions occur at t0

    t0
  [ 3 ]

  Circuit is still not tripped

  ----------------------------------------------------------------------
  t1
  ----------------------------------------------------------------------

  Two more exceptions occur at t1

    t0 t1
  [ 3  2 ]

  Circuit is still not tripped

  ----------------------------------------------------------------------
  t2
  ----------------------------------------------------------------------

  Three more exceptions occur at t2

    t0 t1 t2
  [ 3  2  3 ]

  Circuit is still not tripped

  ----------------------------------------------------------------------
  t3
  ----------------------------------------------------------------------

  At this t0 failures are not counted in total anymore. At t3, 4 more
  exceptions occur

    t1 t2 t3
  [ 2  3  4 ]

  Circuit is still not tripped


  ----------------------------------------------------------------------
  t4
  ----------------------------------------------------------------------

  At this t1 failures are not counted in total anymore. At t4, 3 more
  exceptions occur

    t2 t3 t4
  [ 3  4  3 ]

  Circuit is now tripped

  ----------------------------------------------------------------------
  t4 + timeout
  ----------------------------------------------------------------------

  Circuit is open again

  ----------------------------------------------------------------------

  Params

  timeout (seconds) - The amount of circuit will be kept tripped once it
  trips

  threshold - Number of errors that occur in sliding window that will
  trip the ciruit

  window-size (seconds) - Size of sliding window

  failure-pred - Predicate to classify an exception as a failure "
  [circuit-breaker-name
   {:keys [timeout threshold
           window-size failure-pred]
    :or {timeout 60
         threshold 5
         window-size 60
         failure-pred (constantly true)}}]
  (let [a (atom {:total 0})]
    (map->SlidingWindowCB {:circuit-breaker-name circuit-breaker-name
                           :state a
                           :failure-pred failure-pred
                           :threshold threshold
                           :window-size window-size
                           :timeout timeout})))
