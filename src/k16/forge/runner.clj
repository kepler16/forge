(ns k16.forge.runner
  (:require
   [clj-commons.format.exceptions :as pretty.exceptions]
   [clojure.test :as test]
   [k16.forge.namespace :as forge.namespace]
   [k16.forge.test :as forge.test]
   [lambdaisland.deep-diff2 :as ddiff]
   [puget.printer :as puget])
  (:import
   java.util.concurrent.Executors))

(set! *warn-on-reflection* true)

(defn- compose-fixtures [fixtures]
  (fn global-fixture [runner]
    (reduce (fn [acc fixture]
              (fn [] (fixture acc)))
            runner
            (reverse fixtures))))

(defn- with-global-fixtures [runner]
  (let [composed-fixture (compose-fixtures @forge.test/GLOBAL_FIXTURES)]
    ((composed-fixture runner))))

(defn run-test-ns [test-ns]
  (let [tests (atom {})
        var (atom nil)]
    (with-redefs [test/report
                  (fn [{:keys [type] :as report}]
                    (let [current-var @var]
                      (cond
                        (= type :begin-test-var)
                        (reset! var (:var report))

                        (= type :end-test-var)
                        (reset! var nil)

                        (or (= type :pass)
                            (= type :fail)
                            (= type :error))
                        (swap! tests update current-var
                               (fn [reports]
                                 (conj reports report)))))

                    (when (= (:type report) :pass)
                      (.write System/out (.getBytes ".")))

                    (when (or (= (:type report) :fail)
                              (= (:type report) :error))
                      (.write System/out (.getBytes "F")))

                    (.flush System/out))]

      (try
        (test/test-ns test-ns)
        (catch Exception ex
          (let [current-var @var]
            (swap! tests update current-var
                   (fn [reports]
                     (conj reports {:type :fail
                                    :actual ex}))))))

      @tests)))

(def ^:private puget-opts
  {:print-color true
   :color-scheme {:delimiter nil
                  :tag [:white]

                  :nil [:bold :black]
                  :boolean [:green]
                  :number [:magenta :bold]
                  :string [:bold :green]
                  :character [:bold :magenta]
                  :keyword [:bold :red]
                  :symbol [:white :bold]

                  :function-symbol [:bold :blue]
                  :class-delimiter [:blue]
                  :class-name [:bold :blue]}})

(defn run-all []
  (let [namespaces (forge.namespace/get-test-namespaces)
        pool (Executors/newFixedThreadPool (.availableProcessors (Runtime/getRuntime)))]

    (doseq [ns namespaces]
      (require ns))

    (let [results
          (->> namespaces
               (mapv (fn [test-ns]
                       (.submit pool ^Callable (fn [] (run-test-ns test-ns)))))
               (mapv deref))]

      (println \newline)

      (doseq [result results]
        (doseq [[test reports] result]
          (doseq [report reports]
            (when-not (= :pass (:type report))
              (puget/pprint test puget-opts)

              (cond
                (instance? Exception (:actual report))
                (do (if (:expected report)
                      (do
                       (println "Expected")
                       (puget/pprint (:expected report) puget-opts)
                       (println \newline "Actual"))
                      (println "Failed with exception"))
                    (pretty.exceptions/print-exception (:actual report)))

                (= :matcher-combinators.clj-test/mismatch
                   (-> report :actual meta :type))
                (println (:actual report))

                :else
                (let [diff (ddiff/diff (:expected report)
                                       (:actual report))]
                  (ddiff/pretty-print diff))))))))

    (System/exit 0)))
