(ns k16.forge
  (:refer-clojure :exclude [run!])
  (:require [k16.forge.runner :as runner]))

(defn run! [_]
  (runner/run-all))

(defn -main [& args]
  (runner/run-all))
