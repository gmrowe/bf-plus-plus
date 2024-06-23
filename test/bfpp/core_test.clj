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
    (is (c/error? (c/execute "<"))))
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
    (is (= 81 (c/read-current-memory-cell (with-in-str "Q" (c/execute ","))))))
  (testing
    "If data-pointer is pointing to nonzero a `[` continues to next instruction"
    (is-current-cell-after-exec "+[-]" 0))
  (testing
    "If data-pointer points to zero a `[` advnces code pointer past corresponding `]`"
    (is-current-cell-after-exec "[+]-" -1))
  (testing "If data-pointer points to a `]` it jumps back to corresponding `[`"
    (is-current-cell-after-exec "++++[>+<-]>" 4))
  (testing
    "When code-pointer jumps to closing bracket, nested brackets are skipped"
    (is-current-cell-after-exec "[[]+]" 0))
  (testing "Nested loops jump to correct locations (see comment above)"
    (comment
      "|*A | B | C | = Add numbers in B and C and store the result in A
           >++>+++++<< | Store 2 and 5 in cell B and C
           >[          | Loop on cell B
             <+>       | Increment A
             >[        | Loop on cell C
                <<+>>  | Increment A
                -      | Decrement C
              ]<       | End loop on C
             -         | Decrement A
           ]<          | End loop on B
    ")
    (is-current-cell-after-exec ">++>+++++<<>[<+>>[<<+>>-]<-]<" 7))
  (testing "A `:` prints the value of the current cell as an intger"
    (is (= "65" (with-out-str (with-in-str "A" (c/execute ",:"))))))
  (testing "A `;` takes an integer from input and stores it as an integer"
    (is (= "A" (with-out-str (with-in-str "65" (c/execute ";."))))))
  #_(testing "The internal stack starts out empty" (is (nil? ()))))
