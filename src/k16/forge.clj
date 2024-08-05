(ns k16.forge
  (:gen-class)
  (:require
   [cli-matic.core :as cli]
   [clojure.string :as str]
   [k16.forge.runner :as runner]))

(def ^:private cli-configuration
  {:command "forge"
   :description "A simple test runner for clojure.test"
   :version "0.0.0"
   :opts [{:option "focus"
           :short "f"
           :as "Only run the given test or namespace. Alias for '--include'"
           :type :string}

          {:option "include"
           :short "i"
           :as "Only run the test or namespace described by the given pattern"
           :type :string}

          {:option "exclude"
           :short "e"
           :as "Exclude the test or namespace described by the given pattern from running"
           :type :string}

          {:option "parallelism"
           :short "p"
           :as "The maximum number of tests to run concurrently. Defaults to the number of cores"
           :type :int}]
   :runs
   (fn [props]
     (let [include (or (:include props)
                       (:focus props))
           exclude (:exclude props)
           props (cond-> props
                   include (assoc :include (str/split include #","))
                   exclude (assoc :exclude (str/split exclude #",")))]
       (runner/run-all props)))})

(defn -main [& args]
  (cli/run-cmd args cli-configuration))
