(ns bfpp.core)

(def memsize 16)

(def bf-init-state {:data-pointer 0, :memory (into [] (repeat memsize 0))})

(defn read-current-memory-cell
  [state]
  (nth (:memory state) (:data-pointer state)))

(defn write-current-memory-cell
  [state value]
  (assoc-in state [:memory (:data-pointer state)] value))

(defn with-data-pointer [state f] (update state :data-pointer f))

(defn with-code-pointer [state f] (update state :code-pointer f))

(defn with-current-memory-cell
  [state f]
  (update-in state [:memory (:data-pointer state)] f))

(defn read-current-code-cell
  [state]
  (get-in state [:source-code (:code-pointer state)]))

(defn jump-code-pointer-past-corresponding-closing-bracket
  [state]
  (loop [state (with-code-pointer state inc)
         nesting-level 0]
    (cond (= \] (read-current-code-cell state))
            (if (zero? nesting-level)
              (with-code-pointer state inc)
              (recur (with-code-pointer state inc) (dec nesting-level)))
          (= \[ (read-current-code-cell state))
            (recur (with-code-pointer state inc) (inc nesting-level))
          :else (recur (with-code-pointer state inc) nesting-level))))

(defn jump-code-pointer-to-corresponding-opening-bracket
  [state]
  (loop [state (with-code-pointer state dec)
         nesting-level 0]
    (cond (= \] (read-current-code-cell state))
            (recur (with-code-pointer state dec) (dec nesting-level))
          (= \[ (read-current-code-cell state))
            (if (zero? nesting-level)
              state
              (recur (with-code-pointer state dec) (inc nesting-level)))
          :else (recur (with-code-pointer state dec) nesting-level))))

(defn exec-command
  [state]
  (condp = (read-current-code-cell state)
    \> (-> state
           (with-data-pointer inc)
           (with-code-pointer inc))
    \< (if (pos? (:data-pointer state))
         (-> state
             (with-data-pointer dec)
             (with-code-pointer inc))
         {:error "[Error] Data underflow"})
    \+ (-> state
           (with-current-memory-cell inc)
           (with-code-pointer inc))
    \- (-> state
           (with-current-memory-cell dec)
           (with-code-pointer inc))
    \. (do (-> (read-current-memory-cell state)
               (char)
               (print))
           (with-code-pointer state inc))
    \, (-> state
           (write-current-memory-cell (long (first (read-line))))
           (with-code-pointer inc))
    \[ (if (zero? (read-current-memory-cell state))
         (jump-code-pointer-past-corresponding-closing-bracket state)
         (recur (with-code-pointer state inc)))
    \] (jump-code-pointer-to-corresponding-opening-bracket state)
    \: (do (-> (read-current-memory-cell state)
               (print))
           (with-code-pointer state inc))
    ;; Fall through - just advance the code pointer
    (with-code-pointer state inc)))

(defn execute
  [source-code]
  (loop [state (-> bf-init-state
                   (assoc :source-code (into [] source-code))
                   (assoc :code-pointer 0))]
    (if (and (not (:error state))
             (< (:code-pointer state) (count (:source-code state))))
      (recur (exec-command state))
      state)))
