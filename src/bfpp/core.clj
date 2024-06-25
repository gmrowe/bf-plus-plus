(ns bfpp.core)

(def memsize 16)

(def bf-init-state {:data-pointer 0, :memory (into [] (repeat memsize 0))})

(defn read-mem [state] (nth (:memory state) (:data-pointer state)))

(defn write-mem
  [state value]
  (assoc-in state [:memory (:data-pointer state)] value))

(defn update-mem [state f] (update-in state [:memory (:data-pointer state)] f))

(defn update-data-pointer [state f] (update state :data-pointer f))

(defn update-code-pointer [state f] (update state :code-pointer f))

(defn read-command [state] (get-in state [:source-code (:code-pointer state)]))

(defn jmp-to-close-bracket
  [state]
  (loop [state (update-code-pointer state inc)
         nesting-level 0]
    (condp = (read-command state)
      \] (if (zero? nesting-level)
           (update-code-pointer state inc)
           (recur (update-code-pointer state inc) (dec nesting-level)))
      \[ (recur (update-code-pointer state inc) (inc nesting-level))
      ;; Fall-through
      (recur (update-code-pointer state inc) nesting-level))))

(defn jmp-to-open-bracket
  [state]
  (loop [state (update-code-pointer state dec)
         nesting-level 0]
    (condp = (read-command state)
      \] (recur (update-code-pointer state dec) (dec nesting-level))
      \[ (if (zero? nesting-level)
           state
           (recur (update-code-pointer state dec) (inc nesting-level)))
      ;; Fall-through
      (recur (update-code-pointer state dec) nesting-level))))

(defn exec-command
  [state]
  (condp = (read-command state)
    \> (-> state
           (update-data-pointer inc)
           (update-code-pointer inc))
    \< (if (pos? (:data-pointer state))
         (-> state
             (update-data-pointer dec)
             (update-code-pointer inc))
         {:error "[Error] Buffer underflow"})
    \+ (-> state
           (update-mem inc)
           (update-code-pointer inc))
    \- (-> state
           (update-mem dec)
           (update-code-pointer inc))
    \. (do (-> state
               (read-mem)
               (char)
               (print))
           (update-code-pointer state inc))
    \, (-> state
           (write-mem (long (first (read-line))))
           (update-code-pointer inc))
    \[ (if (zero? (read-mem state))
         (jmp-to-close-bracket state)
         (update-code-pointer state inc))
    \] (if (zero? (read-mem state))
         (update-code-pointer state inc)
         (jmp-to-open-bracket state))
    \: (do (print (read-mem state)) (update-code-pointer state inc))
    \; (-> state
           (write-mem (parse-long (read-line)))
           (update-code-pointer inc))
    \# (-> state
           (update :stack (fnil conj (list)) (read-mem state))
           (update-code-pointer inc))
    \$ (-> state
           (write-mem (peek (:stack state)))
           (update :stack pop)
           (update-code-pointer inc))
    ;; Fall through - just advance the code pointer
    (update-code-pointer state inc)))

(defn error? [state] (some? (:error state)))

(defn execute
  [source-code]
  (loop [{:keys [error source-code code-pointer], :as state}
           (-> bf-init-state
               (assoc :source-code (into [] source-code))
               (assoc :code-pointer 0))]
    (cond (error? state) (do (println error) state)
          (< code-pointer (count source-code)) (recur (exec-command state))
          :else state)))
