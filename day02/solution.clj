(def input (-> (slurp "day02/input.txt")
                                (clojure.string/split-lines)))
(defn sum-value [val increment]
  (if increment (inc val) val))

(defn sum-twos-and-threes [[twos threes] s]
  (let [freq (vals (frequencies s))]
      [(sum-value twos (.contains freq 2)) (sum-value threes (.contains freq 3))]))

(println "Part A:" (reduce * (reduce sum-twos-and-threes [0 0] input)))
