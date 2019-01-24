(defn parse-input [line] 
  (let [res (split-at 1 line)]
       [(symbol (str (ffirst res))) (Integer/parseInt (apply str (last res)))]))

(def input (map parse-input (-> (slurp "day01/input.txt")
                                (clojure.string/split-lines))))
                              
(defn sum-inputs [sum [f s]]
  ((ns-resolve *ns* (symbol (str f))) sum s))

(defn seen-freq [[history sum] [f s]]
  (let [sum (int (sum-inputs sum [f s]))]
    (if (nil? (get history (str sum)))
      [(assoc history (str sum) "seen") sum]
      (reduced sum)))) 

(println "Part A: " (reduce sum-inputs 0 input))
(println "Part B: " (reduce seen-freq [{} 0] (cycle input)))
