(ns tardigrade.timeout)


;; @TODO Add support for threadpool
(defn with-timeout-fn
  [timeout-in-millis body-fn]
  (let [f (future (try [::success (body-fn)]
                       (catch Throwable t
                         [::failure t])))
        [condition v] (deref f
                             timeout-in-millis
                             [::timeout])]
    (case condition
      ::success v
      ::timeout (do (future-cancel f)
                    (throw (ex-info "Timeout exception"
                                    {:timeout-in-millis timeout-in-millis})))
      ::failure (throw v))))


(defmacro with-timeout
  [timeout-in-millis & body]
  `(with-timeout-fn ~timeout-in-millis
     (fn []
       ~@body)))
