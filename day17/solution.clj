(require '[clojure.string :as str]
         '[clojure.walk :as walk]
         '[clojure.pprint :as ppr])

(defn clean-data [line]
  (let [[_ label-1 val-1 label-2 range-1 range-2] line]
    (walk/keywordize-keys {label-1 (read-string val-1) label-2 (range (read-string range-1) (inc (read-string range-2)))})))

(def input (mapv clean-data (re-seq #"(\w)=(\d+), (\w)=(\d+)..(\d+)" (slurp "day17/input.txt"))))

(defn min-max [col]
  (reduce (fn [[min max] val]
            [(if (< val min) val min) (if (> val max) val max)]) 
          [10000 0] col))

(defn find-border-box [data]
  (let [x-vals (flatten (map :x data))  y-vals (flatten (map :y data))]
    (concat (min-max x-vals) (min-max y-vals))))

(def border-box (find-border-box input))
(def x-offset (dec (first border-box)))

(defn create-empty-grid [x y init]
  (mapv (fn [y] (mapv #(init % y) (range x))) (range y)))

(defn print-grid [grid]
  (dorun (map println grid)))

(defn fill-range [fill grid {x :x y :y}]
  (let [new-x (if (seq? x) 
                (if (= (count x) 1) (- (first x) x-offset) (map #(- % x-offset) x))
                (- x x-offset))
        co-ords (if (seq? new-x) 
                  (if (= (count new-x) 1) [[y new-x]] (mapv #(vector y %) new-x)) 
                  (if (seq? y) (mapv #(vector % new-x) y) [[y new-x]]))]
    ; (println fill (count grid) "||" (count (first grid)) new-x (if (coll? x) (map #(- % x-offset) x) x) "||" y)
    (reduce (fn [r c] (assoc-in r c fill)) grid co-ords)))

(def add-clay (partial fill-range "#"))

(defn out-of-bounds [grid [x y]]
  (let [height (count grid) width (count (first grid))]
    (or (or (< x 0) (> x width)) (or (< y 0) (> y height)))))

(defn find-bottom [g loc]
  (loop [grid g [x y] loc]
    (let [next [(inc y) x]
          oob (out-of-bounds grid next)]
      (if (or (out-of-bounds grid next) (= "#" (get-in grid next)))
        [(if oob "oob" "#") y]
        (recur grid [x (inc y)])))))

(defn find-edges [g loc dir]
  (loop [grid g [x y] loc]
    (let [ood (out-of-bounds grid [y (dir x)])
          next (or ood (get-in grid [y (dir x)]))
          under-next (or ood (get-in grid [(inc y) (dir x)]))]
      ; (println dir ood x y next under-next)
      (if (or ood (or (= "#" next) (or (= under-next ".") (= under-next "|"))))
        [next x]
        (recur grid [(dir x) y])
        ))))

; (defn find-edges [g loc dir]
;   (loop [grid g [x y] loc]
;     (let [next-loc [y (dir x)]
;           oob (out-of-bounds grid next-loc)
;           next-val (if (not oob) (get-in grid next-loc))
;           under-loc [(inc y) x]
;           under (if (and (not oob) (not (out-of-bounds grid under-loc))) (get-in grid under-loc))
;           under-next-loc [(inc y) (dir x)]
;           under-next (if (and (not oob) (not (out-of-bounds grid under-next-loc))) (get-in grid under-next-loc))]
;       (if (or oob (= next-val "#") (or (= under-next ".") (= under-next "|")))
;         [next x]
;         (recur grid [(dir x) y])))))

; (defn find-x-path [g loc]
;   )


(defn fill-to-start [g x start-y end-y depth]
  (println "New fill-to-start " x start-y end-y)
  (loop [grid g y start-y]
    (if (or (= y end-y) (out-of-bounds grid [(inc y) x]))
      ; grid
      (if (out-of-bounds grid [(inc y) x]) (fill-range "|" grid {:x (+ x-offset x) :y (range (inc end-y) (inc y))}) grid)
      (let [[lt l] (find-edges grid [x y] dec) [rt r] (find-edges grid [x y] inc)
            walled-in (= "#" lt rt)
            partially-walled (and (not walled-in) (or (= "#" lt) (= "#" rt)))
            even-overflow (and (not walled-in) (not partially-walled) (or (= "~" (get-in grid [(inc y) x])) (= "#" (get-in grid [(inc y) x]))))
            new-start (if (not= l r) (if even-overflow [(inc r) (dec l)] (if partially-walled (if (= "#" lt) (inc r) (dec l)))))
            fill-type (if walled-in "~" "|")
            draw-y y;(if walled-in y (if (or partially-walled even-overflow) y (range (inc end-y) (inc y))))
            draw-x (if (or walled-in partially-walled even-overflow) (range (+ x-offset l) (+ x-offset 1 r)) (+ x-offset x))
            new-bottom (if new-start (if (coll? new-start) (map #(find-bottom grid [% y]) new-start) (find-bottom grid [new-start y])))
            next-step (fill-range fill-type grid {:y draw-y :x draw-x})
            sda (if new-start (println "New Start: " new-start "new-bottom: " new-bottom "x: " x "y " y "draw x: "  (if (coll? draw-x) (map #(- % x-offset) draw-x) draw-x) "depth " depth))
            new-grid (if new-start 
                       (if (coll? new-start) 
                         (fill-to-start 
                          (fill-to-start next-step (last new-start) (last (last new-bottom)) (dec y) (+ 2 depth)) 
                          (first new-start) 
                          (last (first new-bottom)) 
                          (dec y) 
                          (inc depth)) 
                         (fill-to-start next-step new-start (last new-bottom) (dec y) (inc depth))) 
                       next-step)
            ; b (if (= depth 0) (print-grid new-grid))
            
            [[nlt nl] [nrt nr]] [(find-edges new-grid [x (dec y)] dec) (find-edges new-grid [x (dec y)] inc)]
            next-walled-in (= "#" nlt nrt)
            next-partially-walled (and (not next-walled-in) (or (= "#" nlt) (= "#" nrt)) (and (not (nil? nlt)) (not (nil? nrt))))
            next-even-overflow (and (not next-walled-in) (not next-partially-walled) (or (= "~" (get-in new-grid [y x])) (= "#" (get-in new-grid [y x]))))
            ; finish (and new-start (not even-overflow) (not partially-walled) (not next-walled-in))
            finish (and new-start (and (not next-even-overflow) (not next-partially-walled) (not next-walled-in)))
            ; a (if (= depth 0) (println "depth " depth (dec y) nlt nl nrt nr next-walled-in next-partially-walled next-even-overflow "finished " finish))
            a (if (= x 9)
                (do 
                  (println "depth" depth "finish" finish "walled-in" walled-in "partially-walled" partially-walled "even-overflow" even-overflow "new-start" new-start "l" l "r" r "lt" lt "rt" rt "x" x "y" y "draw-x" (if (coll? draw-x) (map #(- % x-offset) draw-x) draw-x) "draw-y" draw-y "start-y" start-y "end-y" end-y)
                  (print-grid grid)
                  (println "")
                  (print-grid new-grid))
                )
            new-y (if (or walled-in partially-walled even-overflow new-start) (dec y) end-y)
            finished-step (if finish (fill-range "|" new-grid {:x (+ x-offset x) :y (range (inc end-y) y)}) new-grid)
            ]
        ; (print-grid next-step)
        (if (= y 1) (println "l" l "r" r "lt" lt "rt" rt "y" y "new-start" new-start "draw-x" draw-x "draw-y" draw-y "walled-in" walled-in "partially-walled" partially-walled "even-overflow" even-overflow))
        ; (if (= depth 0)
        ;   (println l r lt rt y new-start draw-x draw-y walled-in partially-walled even-overflow))
        ; (println "")
        (recur finished-step new-y))
      )))

(time (let [[min-x max-x min-y max-y] border-box
            grid-width (+ (- max-x min-x) 2)
            grid-height (+ (- max-y min-y) 6)
            grid (create-empty-grid grid-width grid-height #(if (and (= %1 (- 500 x-offset)) (= %2 0)) "+" "."))
            with-clay (reduce add-clay grid input)
            bottom (find-bottom with-clay [(- 500 x-offset) 0])
            filled-with-water (fill-to-start with-clay (- 500 x-offset) (last bottom) 0 0)
            ]
        ; (print-grid with-clay)
        (print-grid filled-with-water)
        ; (print-grid (map-indexed #(into [%1] [%2]) filled-with-water))
        (println "Part A: " (count (filter #(or (= "~" %) (= "|" %)) (flatten filled-with-water))))
        ; (println (find-bottom with-clay [(- 500 x-offset) 0]))
        
        ))
