(require '[clojure.string :as str])

(def input (map #(map read-string (str/split %  #", ")) (clojure.string/split-lines (str/trim (slurp "day06/input.txt")))))

(defn min-max [[min-x min-y max-x max-y] [x y]]
  [
   (if (< x min-x) x min-x)
   (if (< y min-y) y min-y)
   (if (> x max-x) x max-x)
   (if (> y max-y) y max-y)
   ])

(defn find-finite-area [coordinates largest [x y]]
  )

(defn find-largest-finite-area [coordinates]
  (reduce (partial find-finite-area coordinates) 0 coordinates))

(let [[min-x min-y max-x max-y] (reduce min-max [1000 1000 0 0] input)]
     ())
