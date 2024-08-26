(ns k16.forge.reporting
  (:require
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
          (puget/pprint test puget-opts)

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
              (ddiff/pretty-print diff))))))))

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
  (puget/pprint summary puget-opts))
