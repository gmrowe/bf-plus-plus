(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test :refer [run-all-tests]]))

(def test-ns)

(defn test-all
  []
  (do
    (refresh)
    (run-all-tests #".*-test")))
