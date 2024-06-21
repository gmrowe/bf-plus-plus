(ns bfpp.core-test
  (:require [bfpp.core :as c]
            [clojure.test :refer [deftest is testing]]))

(defn is-current-cell-after-exec
  [program expected]
  (is (= expected (c/read-current-memory-cell (c/execute program)))))

(deftest interpret-test
  (testing "The data pointer in the initial state points to 0"
    (is (zero? (:data-pointer c/bf-init-state))))
  (testing "The `>` operator increases the data-pointer"
    (is (= 1 (:data-pointer (c/execute ">")))))
  (testing "The `<` operator decreases the data-pointer"
    (is (zero? (:data-pointer (c/execute "><")))))
  (testing "If data-pointer goes negative, an error is thrown"
    (is (some? (:error (c/execute "<")))))
  (testing "All data is zero initialized" (is-current-cell-after-exec "" 0))
  (testing "A `+` increments the value in the current memory cell"
    (is-current-cell-after-exec "+" 1))
  (testing "A `-` decrements the value in the current memory cell"
    (is-current-cell-after-exec "-" -1))
  (testing "The execute command executes all command in a BF++ program"
    (is-current-cell-after-exec "+++-" 2))
  (testing "A `.` converts value in the current memory cell to a char and print"
    (is (= "!"
           (with-out-str (c/execute "+++++++++++++++++++++++++++++++++.")))))
  (testing "After printing with `.` the state is returned"
    (is-current-cell-after-exec "+++++++++++++++++++++++++++++++++." 33))
  (testing
    "A `,` takes char from input and puts its ascii in current memory cell"
    (is (= 81 (c/read-current-memory-cell (with-in-str "Q" (c/execute ",")))))))
