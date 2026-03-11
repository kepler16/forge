(ns k16.forge.reporting
  (:require
   [bling.core :refer [print-bling]]
   [clj-commons.format.exceptions :as pretty.exceptions]
   [lambdaisland.deep-diff2 :as ddiff]
   [puget.printer :as puget]))

(set! *warn-on-reflection* true)

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

(defn print-failures [results]
  (doseq [result results]
    (doseq [[test reports] result]
      (doseq [report reports]
        (when-not (= :pass (:type report))
          (print-bling [:system-red.bold.dashed-underline (subs (str test) 2)])

          (cond
            (instance? Exception (:actual report))
            (do (if (:expected report)
                  (do
                    (println "Expected")
                    (puget/pprint (:expected report) puget-opts)
                    (println \newline "Actual"))
                  (println "Failed with exception"))
                (binding [pretty.exceptions/*print-level* 15]
                  (pretty.exceptions/print-exception (:actual report))))

            (= :matcher-combinators.clj-test/mismatch
               (-> report :actual meta :type))
            (println (:actual report))

            :else
            (let [diff (ddiff/diff (:expected report)
                                   (:actual report))]
              (ddiff/pretty-print diff)))))))

  (print-bling [:system-grey "Failed tests:"])
  (println)

  (doseq [result results]
    (doseq [[test reports] result]
      (doseq [report reports]
        (when-not (= :pass (:type report))
          (print-bling [:system-red.bold (subs (str test) 2)]))))))

(defn calculate-summary [results]
  (reduce
   (fn [acc ns-result]
     (reduce (fn [acc [_ reports]]
               (let [result
                     (reduce
                      (fn [acc report]
                        (if (= :pass (:type report))
                          (update acc :passed inc)
                          (update acc :failed inc)))

                      {:passed 0
                       :failed 0}
                      reports)

                     failed?
                     (> (:failed result) 0)

                     next
                     (-> acc
                         (update-in [:assertions :passed] + (:passed result))
                         (update-in [:assertions :failed] + (:failed result)))]

                 (if failed?
                   (update-in next [:tests :failed] inc)
                   (update-in next [:tests :passed] inc))))

             acc
             ns-result))

   {:tests {:passed 0
            :failed 0}
    :assertions {:passed 0
                 :failed 0}}

   results))

(defn print-summary [summary]
  (print-bling [:bold.system-yellow.dashed-underline "Tests"])
  (print-bling [:system-green "  Passed: "] [:system-green.bold (get-in summary [:tests :passed])])
  (print-bling [:system-red "  Failed: "] [:system-red.bold (get-in summary [:tests :failed])])

  (println)

  (print-bling [:bold.system-yellow.dashed-underline "Assertions"])
  (print-bling [:system-green "  Passed: "] [:system-green.bold (get-in summary [:assertions :passed])])
  (print-bling [:system-red "  Failed: "] [:system-red.bold (get-in summary [:assertions :failed])]))

(defn- format-ms [ms]
  (cond
    (>= ms 1000) (format "%.1fs" (/ ms 1000.0))
    (>= ms 1) (format "%.0fms" ms)
    :else (format "%.2fms" ms)))

(defn- ns-total-ms [{:keys [once tests]}]
  (+ (-> once :setup-ms)
     (-> once :teardown-ms)
     (reduce (fn [acc {:keys [each test-ms]}]
               (+ acc (:setup-ms each) (:teardown-ms each) test-ms))
             0.0
             tests)))

(defn print-timings [timings props]
  (let [all-tests (mapcat :tests timings)
        num-ns (count timings)
        num-tests (count all-tests)
        total-once-setup (reduce #(+ %1 (-> %2 :once :setup-ms)) 0.0 timings)
        total-once-teardown (reduce #(+ %1 (-> %2 :once :teardown-ms)) 0.0 timings)
        total-each-setup (reduce #(+ %1 (:setup-ms (:each %2))) 0.0 all-tests)
        total-each-teardown (reduce #(+ %1 (:teardown-ms (:each %2))) 0.0 all-tests)
        total-test (reduce #(+ %1 (:test-ms %2)) 0.0 all-tests)
        total-once (+ total-once-setup total-once-teardown)
        total-each (+ total-each-setup total-each-teardown)
        avg-once (if (pos? num-ns) (/ total-once num-ns) 0.0)
        avg-each (if (pos? num-tests) (/ total-each num-tests) 0.0)]

    (print-bling [:bold.system-yellow.dashed-underline "Timings"]
                 [:system-grey " "]
                 [:bold.system-blue "Total"]
                 [:system-grey "/"]
                 [:bold.system-green "Setup"]
                 [:system-grey "/"]
                 [:bold.system-red "Teardown"]
                 [:system-grey "/"]
                 [:bold.system-yellow "Average"])
    (print-bling [:system-grey "  Once fixtures:  "]
                 [:system-blue.bold (format-ms total-once)]
                 [:system-grey "/"]
                 [:system-green.bold (format-ms total-once-setup)]
                 [:system-grey "/"]
                 [:system-red.bold (format-ms total-once-teardown)]
                 [:system-grey "/"]
                 [:system-yellow.bold (format-ms avg-once)])
    (print-bling [:system-grey "  Each fixtures:  "]
                 [:system-blue.bold (format-ms total-each)]
                 [:system-grey "/"]
                 [:system-green.bold (format-ms total-each-setup)]
                 [:system-grey "/"]
                 [:system-red.bold (format-ms total-each-teardown)]
                 [:system-grey "/"]
                 [:system-yellow.bold (format-ms avg-each)])
    (print-bling [:system-grey "  Tests:          "]
                 [:system-blue.bold (format-ms total-test)])

    (println)

    (let [sorted-ns (->> timings
                         (sort-by ns-total-ms >)
                         (take 5))]
      (print-bling [:bold.system-yellow.dashed-underline "Slowest namespaces"])
      (doseq [{:keys [ns] :as timing} sorted-ns]
        (print-bling [:system-grey "  "]
                     [:bold.orange (format-ms (ns-total-ms timing))]
                     [:system-grey (str "  " ns)])))

    (println)

    (let [fixture-entries
          (concat
           (->> timings
                (mapcat (fn [{:keys [ns once]}]
                          [{:ns ns :type :once :phase :setup :ms (:setup-ms once)}
                           {:ns ns :type :once :phase :teardown :ms (:teardown-ms once)}])))
           (->> timings
                (mapcat (fn [{:keys [ns tests]}]
                          (mapcat (fn [{:keys [var each]}]
                                    [{:ns ns :var var :type :each :phase :setup :ms (:setup-ms each)}
                                     {:ns ns :var var :type :each :phase :teardown :ms (:teardown-ms each)}])
                                  tests)))))
          sorted-fixtures (->> fixture-entries
                               (sort-by :ms >)
                               (take 5))]
      (print-bling [:bold.system-yellow.dashed-underline "Slowest fixtures"])
      (doseq [{:keys [ns var type phase ms]} sorted-fixtures]
        (let [label (if var
                      (str (subs (str var) 2) " " (name type) " " (name phase))
                      (str ns " " (name type) " " (name phase)))]
          (print-bling [:system-grey "  "]
                       [:bold.orange (format-ms ms)]
                       [:system-grey (str "  " label)]))))

    (when-let [n (:slowest-tests props)]
      (println)
      (let [sorted-tests (->> all-tests
                              (sort-by :test-ms >)
                              (take n))]
        (print-bling [:bold.system-yellow.dashed-underline "Slowest tests"])
        (doseq [{:keys [var test-ms]} sorted-tests]
          (print-bling [:system-grey "  "]
                       [:bold.orange (format-ms test-ms)]
                       [:system-grey (str "  " (subs (str var) 2))]))))))