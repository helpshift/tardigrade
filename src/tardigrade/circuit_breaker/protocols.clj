(ns tardigrade.circuit-breaker.protocols)


(defprotocol Trippable
  (tripped? [this])
  (run-body [this body-fn])
  (reset-trip [this]))
