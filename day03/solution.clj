(require '[clojure.string :as str]
         '[clojure.set :as data])

(def input (->> (slurp "day03/input.txt") (re-seq #"\d+") (partition 5) (map #(map read-string %))))

(defn create-grid [grid [id x y w h]]
  (let [end-x (+ w x)
        end-y (+ h y)
        co-ords (flatten (map (fn [x] (map #(str x ":" %) (range y end-y))) (range x end-x)))]
    (reduce (fn [res key] (let [{ids key} res] (assoc res key (conj ids id)))) grid co-ords)))

(time (let [
            grid (reduce create-grid {} input) 
            overlapping (filter #(> (count %) 1) (vals grid))
            singles (filter #(= (count %) 1) (vals grid))]
        (println "Part A: " (count overlapping))
        (println "Part B: " (first (data/difference (set (flatten singles)) (set (flatten overlapping)))))))
