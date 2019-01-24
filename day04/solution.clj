(require '[clojure.string :as str])

(def input (slurp "day04/input.txt"))
(def parsed-input 
  (->> (re-seq #"\[(\d+)\-(\d+)\-(\d+) (\d+)\:(\d+)\].*(#\d+|wake|sleep)" input)
       (map #(drop 1 %))
       (map (fn [col] (conj (mapv #(Integer/parseInt %) (take 5 col)) (last col))))
       (sort)))

(defn midnight-hour [h m start]
  (if (= h 0) m (if start 0 59)))

(defn guard-sections [result [y m d h m action]]
  (let [{lg :last-guard actions lg :or {actions []}} result]
    (if (re-matches #"#\d+" action)
      (assoc result :last-guard (read-string (str/join "" (drop 1 action))))
      (assoc result lg 
       (conj actions (midnight-hour h m (= "sleep" action)))))))

(defn count-mins [result k v]
  (let [{part-a :part-a part-b :part-b} result
        freq (frequencies (mapcat #(apply range %) (partition 2 v)))
        sum (reduce + 0 (vals freq))
        most-freq-count (apply max (vals freq))
        most-freq (key (apply max-key val freq))
        new-part-a (if (> sum (get part-a :max-sum)) {:max-sum sum :most-time most-freq :id k :result (* k most-freq)} part-a)
        new-part-b (if (> most-freq-count (get part-b :max-freq-count)) {:max-freq-time most-freq :max-freq-count most-freq-count :id k :result (* k most-freq)} part-b)
        ]
    (merge (assoc result (str "#" k) { :freq freq :sum sum }) {:part-a new-part-a :part-b new-part-b})))

(let [grouped-data (dissoc (reduce guard-sections {} parsed-input) :last-guard)
      results (reduce-kv
               count-mins
               {:part-a {:max-sum 0} :part-b {:max-freq-count 0}} 
               grouped-data)
      {{ part-a :result } :part-a { part-b :result } :part-b} results]
  
  (println "Part A: " part-a)
  (println "Part B: " part-b))
