(ns k16.forge.runner
  (:require
   [clojure.test :as test]
   [k16.forge.namespace :as forge.namespace]
   [k16.forge.reporting :as reporting])
  (:import
   java.util.concurrent.Executors))

(set! *warn-on-reflection* true)

(def ^:dynamic *current-test-var* nil)
(def ^:dynamic *reports* nil)

(defn- report-handler [{:keys [type] :as report}]
  (let [current-var @*current-test-var*]
    (cond
      (= type :begin-test-var)
      (reset! *current-test-var* (:var report))

      (= type :end-test-var)
      (reset! *current-test-var* nil)

      (or (= type :pass)
          (= type :fail)
          (= type :error))
      (swap! *reports* update current-var
             (fn [reports]
               (conj reports report)))))

  (when (= (:type report) :pass)
    (.write System/out (.getBytes ".")))

  (when (or (= (:type report) :fail)
            (= (:type report) :error))
    (.write System/out (.getBytes "F")))

  (.flush System/out))

(defn run-test-ns [test-ns]
  (binding [*current-test-var* (atom nil)
            *reports* (atom {})]

    (try
      (test/test-ns test-ns)
      (catch Exception ex
        (let [current-var @*current-test-var*]
          (swap! *reports* update current-var
                 (fn [reports]
                   (conj reports {:type :fail
                                  :actual ex}))))))

    @*reports*))

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

(defn run-all [props]
  (with-redefs [test/report report-handler]
    (let [parallelism (or (:parallelism props)
                          (.availableProcessors (Runtime/getRuntime)))

          namespaces (-> (forge.namespace/get-test-namespaces)
                         (filter-namespaces (:include props) (:exclude props)))
          pool (Executors/newFixedThreadPool parallelism)]

      (doseq [ns namespaces]
        (require ns))

      (let [results
            (->> namespaces
                 (mapv (fn [test-ns]
                         (.submit pool ^Callable (fn [] (run-test-ns test-ns)))))
                 (mapv deref))]

        (println \newline)
        (reporting/print-failures results)
        (reporting/print-summary results))

      (System/exit 0))))
