(ns bfpp.core)

(def memsize 1024)

(def bf-init-state {:data-pointer 0, :memory (into [] (repeat memsize 0))})

(defn read-current-memory-cell
  [state]
  (nth (:memory state) (:data-pointer state)))

(defn write-current-memory-cell
  [state value]
  (assoc-in state [:memory (:data-pointer state)] value))

(defn exec-command
  [state]
  (condp = (get-in state [:source-code (:code-pointer state)])
    \> (-> state
           (update :data-pointer inc)
           (update :code-pointer inc))
    \< (if (pos? (:data-pointer state))
         (-> state
             (update :data-pointer dec)
             (update :code-pointer inc))
         {:error "[Error] Data underflow"})
    \+ (-> state
           (update-in [:memory (:data-pointer state)] inc)
           (update :code-pointer inc))
    \- (-> state
           (update-in [:memory (:data-pointer state)] dec)
           (update :code-pointer inc))
    \. (do (-> (read-current-memory-cell state)
               (char)
               (print))
           (update state :code-pointer inc))
    \, (-> state
           (write-current-memory-cell (long (first (read-line))))
           (update :code-pointer inc))))

(defn execute
  [source-code]
  (loop [state (-> bf-init-state
                   (assoc :source-code (into [] source-code))
                   (assoc :code-pointer 0))]
    (if (and (not (:error state))
             (< (:code-pointer state) (count (:source-code state))))
      (recur (exec-command state))
      state)))
