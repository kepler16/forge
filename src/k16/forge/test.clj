(ns k16.forge.test)

(def GLOBAL_FIXTURES
  (atom []))

(defn with-global-fixture [fixture]
  (swap! GLOBAL_FIXTURES conj fixture))
