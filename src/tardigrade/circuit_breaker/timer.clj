(ns tardigrade.circuit-breaker.timer
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

(defrecord TimerCB
    [circuit-breaker-name state
     threshold timeout window-size
     threshold-time body-fn-timeout]
  tcp/Trippable
  (tripped? [this]
    (circuit-tripped? @state timeout))
  (run-body [this body-fn]
    (let [f (future
              (let [dt (time/now)
                    res (try [::success
                              (body-fn)
                              dt
                              (time/now)]
                             (catch Throwable t
                               [::failure t dt (time/now)]))]
                res))
          [res-t res dt1 dt2] (deref f body-fn-timeout [::timeout])]
      (case res-t
        ::timeout (do (trip-failure-hit state
                                        window-size
                                        threshold)
                      [::tcp/failure (ex-info "Circuit breaker - body timeout"
                                              {:cb-name circuit-breaker-name})])
        ::failure [::tcp/failure res]
        ::success (if (>= (time/in-millis (time/interval dt1 dt2))
                          threshold-time)
                    (do (trip-failure-hit state
                                          window-size
                                          threshold)
                        [::tcp/success res])
                    [::tcp/success res]))))
  (reset-trip [this]
    (trip-failure-reset state)))


(defn create-timer-trippable
  [circuit-breaker-name
   {:keys [timeout threshold window-size threshold-time body-fn-timeout]
    :or {timeout 60
         window-size 60
         threshold 5
         threshold-time 60
         body-fn-timeout 300}}]
  (map->TimerCB {:circuit-breaker-name circuit-breaker-name
                 :state (atom {:total 0})
                 :threshold threshold
                 :window-size window-size
                 :timeout timeout
                 :threshold-time threshold-time
                 :body-fn-timeout (* body-fn-timeout 1000)}))
