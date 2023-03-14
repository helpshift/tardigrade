(ns tardigrade.retry
  "Retry utilites")

(defn with-retry-fn
  "sleep-seq - Sequence of times in millis. Used to sleep before every retry
   failure-pred - Predicate that will decide if to retry or not
   body-fn - Code thunk"
  [sleep-seq failure-pred body-fn]
  (let [[status res] (try
                       [::success (body-fn)]
                       (catch Throwable e
                         [::failure e]))]
    (if (= status ::success)
      res
      (if (and (failure-pred res)
               (seq sleep-seq))
        (let [[sleep & rest-sleep-seq] sleep-seq]
          (Thread/sleep sleep)
          (recur rest-sleep-seq
                 failure-pred
                 body-fn))
        (throw res)))))


(defmacro with-retries
  "sleep-seq - Sequence of times in millis. Used to sleep before every retry
   failure-pred - Predicate that will decide if to retry or not"
  [sleep-seq failure-pred & body]
  `(with-retry-fn ~sleep-seq ~failure-pred
     (fn []
       ~@body)))
