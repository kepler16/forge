(ns k16.forge.runner
  (:require
   [bling.core :as bling :refer [print-bling]]
   [clojure.test :as test]
   [k16.forge.namespace :as forge.namespace]
   [k16.forge.reporting :as reporting])
  (:import
   java.util.concurrent.Executors
   java.util.concurrent.ExecutorService
   java.util.concurrent.Semaphore
   java.util.concurrent.TimeUnit))

(set! *warn-on-reflection* true)

(def ^:dynamic *reports* nil)

(defmacro vthread
  {:style/indent :defn}
  [^ExecutorService executor & body]
  `(let [^Callable fn# (bound-fn [] ~@body)]
     (ExecutorService/.submit ~executor fn#)))

(defn- report-handler [{:keys [type] :as report}]
  (when (or (= type :pass)
            (= type :fail)
            (= type :error))
    (let [current-var (first test/*testing-vars*)]
      (swap! *reports* update current-var
             (fn [reports]
               (conj reports report)))))

  (when (= type :pass)
    (.write System/out (.getBytes ".")))

  (when (or (= type :fail)
            (= type :error))
    (.write System/out (String/.getBytes (bling/bling [:bold.system-red "F"]))))

  (.flush System/out))

(defn- get-test-vars [ns]
  (->> (ns-interns ns)
       vals
       (filter #(:test (meta %)))))

(defn- get-ns-fixtures [ns]
  (let [ns (the-ns ns)
        ns-meta (meta ns)]
    {:once (test/join-fixtures (::test/once-fixtures ns-meta))
     :each (test/join-fixtures (::test/each-fixtures ns-meta))}))

(defn- run-test-var [v each-fixture-fn results]
  (binding [*reports* (atom {})]
    (try (each-fixture-fn (fn [] (test/test-var v)))
         (catch Exception ex
           (swap! *reports* update v
                  (fn [reports]
                    (conj reports {:type :fail
                                   :var v
                                   :actual ex})))))
    (let [result @*reports*]
      (when (seq result)
        (swap! results conj result)))))

(defn- run-test-ns
  [^ExecutorService executor ^Semaphore semaphore test-ns results interrupted?]
  (let [test-vars (get-test-vars test-ns)
        {:keys [once each]} (get-ns-fixtures test-ns)
        released? (atom false)]

    (Semaphore/.acquire semaphore)
    (try
      (when-not @interrupted?
        (once
         (fn []
           (Semaphore/.release semaphore)
           (reset! released? true)
           (->> test-vars
                (mapv (fn [v]
                        (vthread executor
                          (Semaphore/.acquire semaphore)
                          (try (when-not @interrupted?
                                 (run-test-var v each results))
                               (finally
                                 (Semaphore/.release semaphore))))))

                (mapv deref)))))

      (finally
        (when-not @released?
          (Semaphore/.release semaphore))))))

(defn- contains-pattern? [sym patterns]
  (reduce
   (fn [_ pattern]
     (let [pattern (re-pattern pattern)]
       (if (re-find pattern (str sym))
         (reduced true)
         false)))
   false
   patterns))

(defn- filter-namespaces [namespaces include exclude]
  (filter
   (fn [namespace]
     (let [included (if (seq include)
                      (contains-pattern? namespace include)
                      true)
           excluded (if (seq exclude)
                      (contains-pattern? namespace exclude)
                      false)]
       (and included (not excluded))))
   namespaces))

(defn- print-results [results]
  (let [summary (reporting/calculate-summary results)
        failed? (< 0 (-> summary :tests :failed))]
    (println \newline)
    (reporting/print-failures results)
    (println)
    (reporting/print-summary summary)
    failed?))

(defn run-all [props]
  (with-redefs [test/report report-handler]
    (let [parallelism (or (:parallelism props)
                          (.availableProcessors (Runtime/getRuntime)))

          namespaces (-> (forge.namespace/get-test-namespaces)
                         (filter-namespaces (:include props) (:exclude props)))

          ^ExecutorService executor (Executors/newVirtualThreadPerTaskExecutor)
          semaphore (Semaphore. parallelism)

          results (atom [])
          interrupted? (atom false)

          shutdown-hook
          (Thread.
           (fn []
             (reset! interrupted? true)
             (.shutdown executor)
             (println (bling/bling "["
                                   [:bold.system-red "Interrupted"]
                                   "::Waiting for in-progress tests to terminate]"))
             (when-not (.awaitTermination executor 10 TimeUnit/SECONDS)
               (.shutdownNow executor))
             (print-results @results)
             (println)))]

      (print-bling [:system-yellow.bold "Loading test namespaces "]
                   [:system-blue.bold (str "[" (count namespaces) "]")]
                   [:system-yellow.bold " ..."])
      (doseq [ns namespaces]
        (require ns))

      (.addShutdownHook (Runtime/getRuntime) shutdown-hook)

      (->> namespaces
           (mapv (fn [test-ns]
                   (vthread executor
                     (when-not @interrupted?
                       (run-test-ns executor semaphore
                                    test-ns results interrupted?)))))
           (mapv deref))

      (when-not @interrupted?
        (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
             (catch IllegalStateException _))
        (.close executor)
        (let [failed? (print-results @results)]
          (System/exit (if failed? 1 0)))))))
