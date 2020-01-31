(ns pe.001)

(defn mult-of-3or5?
  [n]
  (let [f (fn [d] (= 0 (rem n d)))]
    (or (f 5) (f 3)))) 

(defn solve
  "using brute force"
  [n]
  (reduce + (filter mult-of-3or5? (range 1 n))))

(solve 1000)

(defn mult-sum
  [n d]
  (let [m (quot n d)]
    (* (quot (* m (+ m 1)) 2) d))) 

(defn solve2
  "using math"
  [n]
  (let [f (partial mult-sum (- n 1))]
    (+ (f 3) (f 5) (- (f 15)))))

(solve2 1000)

(defn -main [] (println (solve2 1000)))
