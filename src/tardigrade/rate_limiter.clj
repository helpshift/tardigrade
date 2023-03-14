(ns tardigrade.timeout
  (:require [clj-time.core :as ct]
            [clojure.core.async :as async]))


(defn rate-limited-channel
    [rate-atom]
    (let [c (async/chan (async/buffer 1))]
      (async/thread
        (try
          (loop [i 0
                 last-ts (ct/now)]
            (let [f (async/<!! c)
                  interval (/ 1000000000 @rate-atom)
                  t (System/nanoTime)]
              (when f
                (future (f))
                (let [start (System/nanoTime)]
                  (loop [end (System/nanoTime)]
                    (when (> (+ start interval) end)
                      (recur (System/nanoTime))))

                  (let [ts (ct/now)
                        interval (try (ct/in-seconds (ct/interval ts last-ts))
                                      (catch Throwable e
                                        ))]
                    (if (and interval
                             (>= interval 1))
                      (recur 0
                             ts)
                      (recur (inc i)
                             last-ts)))))))
          (catch Throwable t
            ;; @TODO Add log statement
            )))
      c))
