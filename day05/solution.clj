(require '[clojure.string :as str])

(def input (str/trim (slurp "day05/input.txt")))

(defn reaction [a b]
  (and (not= a b) (= (str/lower-case a) (str/lower-case b))))

(defn remove-last [s]
  (subs s 0 (- (count s) 1)))

(defn inc-count [obj char]
  (if (nil? (get-in obj (str/lower-case char)))
    (assoc-in obj (str/lower-case char) 1)
    (update obj (first (str/lower-case char)) inc)))

(defn append-non-reaction [result char]
  (let [last-char (last result)]
    (if (and last-char (reaction last-char char))
         (remove-last result)
         (str result char))))

(defn filter-char [filter e]
  (not= (str/lower-case filter) (str/lower-case e)))

(let [result (reduce append-non-reaction "" input)]
  (println "Part A: " (count result))
  ; (println "Part B: " (reduce min (map count (map #(filter (partial filter-char %) "dabAcCaCBAcCcaDA") (seq "abcdefghijklmnopqrstuvwxyz"))))))
  (println "Part B: " (map #(filter (partial filter-char %) "dabAcCaCBAcCcaDA") (seq "abcdefghijklmnopqrstuvwxyz"))))
